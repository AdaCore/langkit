## vim: filetype=makoada

<%def name="argument_list(property, dispatching)">
  (${property.self_arg_name} : ${Self.type.name}
   % for arg in property.arguments:
      ; ${arg.name} : ${arg.type.name}
      % if arg.default_value is not None:
         := ${arg.default_value.render_private_ada_constant()}
      % endif
   % endfor
   % if property.uses_entity_info:
   ; ${property.entity_info_name} : ${T.entity_info.name} :=
      ${T.entity_info.nullexpr}
   % endif
  )
</%def>

<%def name="dynamic_vars_holder_decl(type_name, base_type_name, dynvars)">
   type ${type_name} is new ${base_type_name} with
   % if dynvars:
      record
         % for dynvar in dynvars:
            ${dynvar.argument_name} : ${dynvar.type.name};
         % endfor
      end record;
   % else:
      null record;
   % endif
</%def>

<%def name="logic_functor(prop, arity)">
   <%
      type_name = f"Logic_Functor_{prop.uid}"
      entity = T.entity.name
      is_dynamic = prop.is_dynamic_combiner
      is_converter = arity == 1 and not is_dynamic

      base_type: str
      subp_name: str
      error_name: str
      if is_converter:
         base_type = "Converter_Type"
         subp_name = "Convert"
         error_name = "conv_prop"
      else:
         base_type = "Combiner_Type"
         subp_name = "Combine"
         error_name = "comb_prop"

      args = [f"Self : {type_name}"]
      args.append(
         f"From : {entity}"
         if is_converter else
         f"Vals : Entity_Vars.Value_Array"
      )
      subp_spec = (
         f"overriding function {subp_name} ({'; '.join(args)}) return {entity}"
      )

      has_multiple_concrete_nodes = len(T.root_node.concrete_subclasses) > 1
   %>

   ${dynamic_vars_holder_decl(
         type_name, f"Solver_Ifc.{base_type}", prop.dynamic_vars
   )}

   ${subp_spec} with Inline;
   overriding function Image (Self : ${type_name}) return String;

   ${subp_spec} is
      % if not prop.dynamic_vars:
         pragma Unreferenced (Self);
      % endif

      ## If there is no From argument, create a local variable so that code
      ## generation below can always refer to the controlling argument as
      ## "From".
      % if not is_converter:
         From : constant ${T.entity.name} := Vals (1);
      % endif

      % if is_dynamic:
         <% arr_arg = prop.natural_arguments[0] %>
         Args : ${arr_arg.type.name} :=
           ${arr_arg.type.constructor_name} (Vals'Length - 1);
      % endif

      <%
         # Range of "Vals" indexes for the entity arguments to pass to the
         # property in addition to "Self".
         #
         # Index 1 is for "Self", so the other entity arguments start at index
         # 2.
         extra_entity_args_range = range(2, arity + 1)

         # List of all entity arguments ("Self" included) to pass to the
         # property, plus their expected types.
         typed_entity_args = [
            ("From", prop.struct),
         ] + [
            (f"Vals ({i})", arg.type.element_type)
            for i, arg in zip(extra_entity_args_range, prop.natural_arguments)
         ]
      %>

      Ret : ${prop.type.name};
   begin
      ## Entities passed as arguments can contain any node type: make sure
      ## their types match the expected types for "prop"'s arguments before
      ## doing the call so that we can raise a Property_Error exception right
      ## now instead of letting Ada raise an automatic Constraint_Error.
      ##
      ## No need to perform the check if the expected kind is the root node or
      ## if there is only one concrete node in the whole language, else get a
      ## compilation warning.
      % for arg, expected_type in typed_entity_args:
         % if has_multiple_concrete_nodes and expected_type != T.root_node:
            if ${arg}.Node /= null
               and then ${arg}.Node.Kind not in
                 ${expected_type.ada_kind_range_name}
            then
               Raise_Property_Exception
                 (From.Node,
                  Property_Error'Identity,
                  "mismatching node type for ${error_name}");
            end if;
         % endif
      % endfor

      % if is_dynamic:
      <% expected_type = arr_arg.type.element_type.element_type %>
      for I in 2 .. Vals'Last loop
         % if has_multiple_concrete_nodes and expected_type != T.root_node:
            if Vals (I).Node /= null
               and then Vals (I).Node.Kind not in
                  ${expected_type.ada_kind_range_name}
            then
               Raise_Property_Exception
                 (Vals (I).Node,
                  Property_Error'Identity,
                  "mismatching node type for ${error_name}");
            end if;
         % endif
         Args.Items (I - 1) := (Vals (I).Node, Vals (I).Info);
      end loop;
      % endif

      ## Here, we just forward the return value from prop to our caller, so
      ## there is nothing to do regarding ref-counting.
      <%
         # Pass the property controlling node argument
         args = [f"{prop.self_arg_name} => From.Node"]

         if is_dynamic:
            args.append(f"{arr_arg.name} => Args")

         # Pass other entity arguments. Pass an aggregate as the property may
         # take a non-root entity type, while "Vals" contains only root
         # entities.
         for i, vals_index in enumerate(extra_entity_args_range):
            args.append(
               f"{prop.natural_arguments[i].name} =>"
               f" (Node => Vals ({vals_index}).Node,"
               f"  Info => Vals ({vals_index}).Info)"
            )

         # Pass all the required dynamic variables from the closure
         for dynvar in prop.dynamic_vars:
            args.append(
               f"{dynvar.argument_name} => Self.{dynvar.argument_name}"
            )

         # Pass the entity info argument, if the property takes one
         if prop.uses_entity_info:
            args.append(f"{prop.entity_info_name} => From.Info")
      %>
      Ret := ${prop.name} (${", ".join(args)});

      % if is_dynamic:
      Dec_Ref (Args);
      % endif;

      return (Node => Ret.Node, Info => Ret.Info);
   % if is_dynamic:
   exception
      when Exc : Property_Error =>
         pragma Unreferenced (Exc);
         Dec_Ref (Args);
         raise;
   % endif
   end ${subp_name};

   -----------
   -- Image --
   -----------

   overriding function Image (Self : ${type_name}) return String is
   begin
      return (${ascii_repr(prop.qualname)});
   end Image;
</%def>

<%def name="logic_predicates(prop)">
   % for pred in prop.logic_predicates:

   <%
      type_name = f"{pred.id}_Predicate"
      package_name = f"{pred.id}_Pred"
      formal_node_types = prop.get_concrete_node_types(pred)
      enumerated_arg_types = list(enumerate(formal_node_types[1:], 1))
      arity = len(formal_node_types)
      has_refcounted_args = any(
         pa.type.is_refcounted for pa in pred.partial_args
      )
      has_multiple_concrete_nodes = len(T.root_node.concrete_subclasses) > 1
   %>

   <%def name="call_profile()">
      overriding function Call
        (Self : ${type_name};
         % if arity == 1:
            Entity : ${T.entity.name}
         % else:
            Entities : Entity_Vars.Value_Array
         % endif
        ) return Boolean
   </%def>

   <%def name="failed_profile()">
      overriding procedure Failed
        (Self : ${type_name};
         % if arity == 1:
            Entity : ${T.entity.name};
         % else:
            Entities : Entity_Vars.Value_Array;
         % endif
         Ctxs    : Solver_Ifc.Logic_Context_Array;
         Round   : Natural;
         Emitter : Solver_Ifc.Diagnostic_Emitter
        )
   </%def>

   type ${type_name} is
   new Solver_Ifc.${"Predicate_Type" if arity == 1 else "N_Predicate_Type"}
   with record
      % for pa in pred.partial_args:
         Field_${pa.index} : ${pa.type.name};
      % endfor
      % if prop.predicate_error is not None:
         Error_Location : ${T.root_node.name};
      % elif not pred.partial_args:
         null;
      % endif
   end record;

   ${call_profile()};

   % if prop.predicate_error is not None:
   ${failed_profile()};
   % endif

   overriding function Image (Self : ${type_name}) return String;
   % if has_refcounted_args:
      overriding procedure Destroy (Self : in out ${type_name});
   % endif

   <%
      create_args = [(f"Field_{pa.index}", pa.type)
                     for pa in pred.partial_args]
      if prop.predicate_error is not None:
        create_args.append(("Error_Location", T.root_node))
   %>

   function Create_${pred.id}_Predicate
   % if create_args:
   (
      % for arg_name, arg_type in create_args:
         ${arg_name} : ${arg_type.name}${"" if loop.last else ";"}
      % endfor
   )
   % endif
      return ${type_name} is
   begin
      <%
         components = []
         if arity > 1:
            components.append(f"N => {arity}")
         components += [
            "Cache_Set => False",
            "Cache_Key => <>",
            "Cache_Value => <>",
            "Ref_Count => 1",
         ]
      %>
      % for arg_name, arg_type in create_args:
         % if arg_type.is_refcounted:
            Inc_Ref (${arg_name});
         % endif
         <% components.append(f"{arg_name} => {arg_name}") %>
      % endfor
      return ${type_name}'(${", ".join(components)});
   end;

   ----------
   -- Call --
   ----------

   ${call_profile()}
   is
      % if not pred.partial_args:
         pragma Unreferenced (Self);
      % endif

      % if arity > 1:
         Entity : ${T.entity.name} := Entities (1);
      % endif
      <% node0_type = formal_node_types[0] %>
      Node : ${node0_type.name};
   begin
      ## Here, we'll raise a property error, but only for dispatching
      ## properties. For non dispatching properties we'll allow the user
      ## property to handle null however it wants.
      % if prop.dispatching and not ctx.no_property_checks:
         if Node_0.Node = null then
            Raise_Property_Exception
              (Node_0.Node,
               Property_Erro'Identity,
               "In predicate, calling dispatching property on a null node");
        end if;
      % endif

      ## Type check nodes that come from logic vars to avoid Assertion_Error or
      ## Assertion_Error in case of mismatch.
      % if not ctx.emitter.no_property_checks:
         <%
            typed_nodes = (
               [("Entity.Node", prop.struct)]
               + [
                  (f"Entities ({i + 1}).Node", t.element_type)
                  for i, t in enumerated_arg_types
               ]
            )
         %>
         % for node_expr, node_type in typed_nodes:
            % if has_multiple_concrete_nodes and not node_type.is_root_node:
               if ${node_expr} /= null
                  and then ${node_expr}.Kind
                           not in ${node_type.ada_kind_range_name}
               then
                  Raise_Property_Exception
                    (Node, Property_Error'Identity, "mismatching node type");
               end if;
            % endif
         % endfor
      % endif

      Node := Entity.Node;

      ## Pass the "prefix" node (the one that owns the property to call) using
      ## the conventional bare node/entity_info arguments. Pass the other node
      ## arguments as entities directly.
      <%
         args = ['Node'] + [
            f"(Node => Entities ({i + 1}).Node,"
            f" Info => Entities ({i + 1}).Info)"
            for i, formal_type in enumerated_arg_types
         ] + [
            f"{pa.name} => Self.Field_{pa.index}" for pa in pred.partial_args
         ]
         if prop.uses_entity_info:
            args.append(f"{prop.entity_info_name} => Entity.Info")
         args_fmt = '({})'.format(', '.join(args)) if args else ''
      %>
      return ${prop.name} ${args_fmt};
   end Call;

   % if prop.predicate_error is not None:
   ------------
   -- Failed --
   ------------

   ${failed_profile()}
   is
      <%
         template, args = prop.predicate_error_diagnostic(arity)
      %>
      Args : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (${len(args)});

      Contexts : Internal_Logic_Context_Array_Access :=
         Create_Internal_Logic_Context_Array (Ctxs'Length);

      Diag : constant Internal_Solver_Diagnostic :=
        (Message_Template => Create_String ("${template}"),
         Args             => Args,
         Contexts         => Contexts,
         Location         => Self.Error_Location,
         Round            => Round);
   begin
      % for i, arg in enumerate(args):
      Args.Items (${i + 1}) := ${arg};
      % endfor
      for I in Ctxs'Range loop
         Contexts.Items (I) := Ctxs (I).all;
      end loop;
      Emitter (Diag);
   end Failed;
   % endif

   -----------
   -- Image --
   -----------

   overriding function Image (Self : ${type_name}) return String is
   begin
      return ${ascii_repr(prop.qualname)};
   end Image;

   % if has_refcounted_args:
      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Self : in out ${type_name}) is
      begin
         % for pa in pred.partial_args:
            % if pa.type.is_refcounted:
               Dec_Ref (Self.Field_${pa.index});
            % endif
         % endfor
         null;
      end Destroy;
   % endif

   % endfor
</%def>

<%def name="inc_ref(var)">
   % if var.type.is_refcounted:
      Inc_Ref (${var.name});
   % endif
</%def>
