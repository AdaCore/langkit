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

      base_type: str
      subp_name: str
      error_name: str
      if arity == 1:
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
         if arity == 1 else
         f"Vals : Entity_Vars.Value_Array"
      )
      subp_spec = (
         f"overriding function {subp_name} ({'; '.join(args)}) return {entity}"
      )
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
      % if arity > 1:
         From : constant ${T.entity.name} := Vals (1);
      % endif

      <%
         # Range of "Vals" indexes for the entity arguments to pass to the
         # property in addition to "Self".
         #
         # Index 1 is for "Self", so the other entity arguments start at index
         # 2.
         extra_entity_args_range = range(2, arity + 1)

         # List of all entity arguments ("Self" included) to pass to the
         # property, plus their expected types as node kind ranges.
         typed_entity_args = [
            ("From", prop.struct.ada_kind_range_name),
         ] + [
            (f"Vals ({i})", arg.type.element_type.ada_kind_range_name)
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
      ## No need to perform the check if there is only one concrete node in the
      ## whole language, or we get a compilation warning.
      % if len(T.root_node.concrete_subclasses) > 1:
         % for arg, kind_range in typed_entity_args:
            if ${arg}.Node /= null
               and then ${arg}.Node.Kind not in ${kind_range}
            then
               Raise_Property_Exception
                 (From.Node,
                  Property_Error'Identity,
                  "mismatching node type for ${error_name}");
            end if;
         % endfor
      % endif

      ## Here, we just forward the return value from prop to our caller, so
      ## there is nothing to do regarding ref-counting.
      <%
         # Pass the property controlling node argument
         args = [f"{prop.self_arg_name} => From.Node"]

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

      return (Node => Ret.Node, Info => Ret.Info);
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
   % for (args_types, default_passed_args, pred_id) in prop.logic_predicates:

   <%
      type_name = f"{pred_id}_Predicate"
      package_name = f"{pred_id}_Pred"
      formal_node_types = prop.get_concrete_node_types(args_types,
                                                       default_passed_args)
      arity = len(formal_node_types)
      refcounted_args_types = filter(lambda t: t.is_refcounted, args_types)
      args = list(enumerate(args_types))
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

   type ${type_name} is
   new Solver_Ifc.${"Predicate_Type" if arity == 1 else "N_Predicate_Type"}
   with record
      % for i, arg_type in args:
         Field_${i} : ${arg_type.name};
      % endfor
      % if not args:
         null;
      % endif
   end record;

   ${call_profile()};
   overriding function Image (Self : ${type_name}) return String;
   % if refcounted_args_types:
      overriding procedure Destroy (Self : in out ${type_name});
   % endif

   function Create_${pred_id}_Predicate
   % if args:
   (
      % for i, arg_type in args:
         Field_${i} : ${arg_type.name}${"" if loop.last else ";"}
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
      % for i, arg_type in args:
         % if arg_type.is_refcounted:
            Inc_Ref (Field_${i});
         % endif
         <% components.append(f"Field_{i} => Field_{i}") %>
      % endfor
      return ${type_name}'(${", ".join(components)});
   end;

   ----------
   -- Call --
   ----------

   ${call_profile()}
   is
      % if not args_types:
         pragma Unreferenced (Self);
      % endif

      % if arity > 1:
         Entity : ${T.entity.name} := Entities (1);
      % endif
      <% node0_type = formal_node_types[0] %>
      Node : constant ${node0_type.name} := Entity.Node;
   begin
      ## Here, we'll raise a property error, but only for dispatching
      ## properties. For non dispatching properties we'll allow the user to
      ## handle null however he wants.
      % if prop.dispatching and not ctx.no_property_checks:
         if Node_0.Node = null then
            Raise_Property_Exception
              (Node_0.Node,
               Property_Erro'Identity,
               "In predicate, calling dispatching property on a null node");
        end if;
      % endif

      ## Pass the "prefix" node (the one that owns the property to call) using
      ## the conventional bare node/entity_info arguments. Pass the other node
      ## arguments as entities directly.
      <%
         args = ['Node'] + [
            f"(Node => Entities ({i + 1}).Node,"
            f" Info => Entities ({i + 1}).Info)"
            for i, formal_type in enumerate(formal_node_types[1:], 1)
      ] + [f"Self.Field_{i}" for i, _ in enumerate(args_types)]
         if prop.uses_entity_info:
            args.append(f"{prop.entity_info_name} => Entity.Info")
         args_fmt = '({})'.format(', '.join(args)) if args else ''
      %>
      return ${prop.name} ${args_fmt};
   end Call;

   -----------
   -- Image --
   -----------

   overriding function Image (Self : ${type_name}) return String is
   begin
      return ${ascii_repr(prop.qualname)};
   end Image;

   % if refcounted_args_types:
      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Self : in out ${type_name}) is
      begin
         % for i, arg_type in enumerate(refcounted_args_types):
            Dec_Ref (Self.Field_${i});
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
