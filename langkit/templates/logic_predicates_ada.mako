## vim: filetype=makoada

<%def name="create_profile(pred)">
   <%
      args = []
      if pred.arity > 1:
          args.append(("N", "Positive"))
      for pa in pred.partial_args:
          args.append((pa.name, pa.type.name))
      if pred.prop.predicate_error is not None:
          args.append(("Error_Location", T.root_node.name))
   %>

   function ${pred.constructor_name}
   % if args:
   (
      % for arg_name, arg_type in args:
         ${arg_name} : ${arg_type}${"" if loop.last else ";"}
      % endfor
   )
   % endif
      return ${pred.type_name}
</%def>

<%def name="call_profile(pred)">
   overriding function Call
     (Self : ${pred.type_name};
      % if pred.arity == 1:
         Entity : ${T.entity.name}
      % else:
         Entities : Entity_Vars.Value_Array
      % endif
     ) return Boolean
</%def>

<%def name="failed_profile(pred)">
   overriding procedure Failed
     (Self : ${pred.type_name};
      % if pred.arity == 1:
         Entity : ${T.entity.name};
      % else:
         Entities : Entity_Vars.Value_Array;
      % endif
      Ctxs    : Solver_Ifc.Logic_Context_Array;
      Round   : Natural;
      Emitter : Solver_Ifc.Diagnostic_Emitter)
</%def>

<%def name="decl(pred)">
   type ${pred.type_name} is new Solver_Ifc.${pred.base_type} with record
      % for pa in pred.partial_args:
         Field_${pa.index} : ${pa.type.name};
      % endfor
      % if pred.prop.predicate_error is not None:
         Error_Location : ${T.root_node.name};
      % elif not pred.partial_args:
         null;
      % endif
   end record
   with First_Controlling_Parameter;

   ${create_profile(pred)};

   ${call_profile(pred)};

   % if pred.prop.predicate_error is not None:
   ${failed_profile(pred)};
   % endif

   overriding function Image (Self : ${pred.type_name}) return String;
   % if pred.has_refcounted_args:
      overriding procedure Destroy (Self : in out ${pred.type_name});
   % endif
</%def>

<%def name="body(pred)">

   <%
      prop = pred.prop
      enumerated_arg_types = list(enumerate(pred.formal_node_types[1:], 1))
      has_multiple_concrete_nodes = len(T.root_node.concrete_subclasses) > 1
   %>

   ${create_profile(pred)} is
   begin
      <%
         components = []
         if pred.arity > 1:
            components.append("N => N")
         components += [
            "Cache_Set => False",
            "Cache_Key => <>",
            "Cache_Value => <>",
            "Ref_Count => 1",
         ]
         if prop.predicate_error is not None:
             components.append("Error_Location => Error_Location")
      %>
      % for pa in pred.partial_args:
         % if pa.type.is_refcounted:
            Inc_Ref (${pa.name});
         % endif
         <% components.append(f"Field_{pa.index} => {pa.name}") %>
      % endfor
      return ${pred.type_name}'(${", ".join(components)});
   end ${pred.constructor_name};

   ----------
   -- Call --
   ----------

   ${call_profile(pred)}
   is
      % if not pred.partial_args:
         pragma Unreferenced (Self);
      % endif

      % if pred.arity > 1:
         Entity : ${T.entity.name} := Entities (1);
      % endif

      % if pred.is_variadic:
         <% arr_arg = prop.natural_arguments[0] %>
         Args : ${arr_arg.type.name} :=
           ${arr_arg.type.constructor_name} (Entities'Length - 1);
      % endif

      <% node0_type = pred.formal_node_types[0] %>
      Node : ${node0_type.name};

      Ret : Boolean;
   begin
      ## Here, we'll raise a property error, but only for dispatching
      ## properties. For non dispatching properties we'll allow the user
      ## property to handle null however it wants.
      % if prop.dispatching:
         if Node_0.Node = null then
            Raise_Property_Exception
              (Node_0.Node,
               Property_Erro'Identity,
               "In predicate, calling dispatching property on a null node");
        end if;
      % endif

      ## Type check nodes that come from logic vars to avoid Assertion_Error or
      ## Assertion_Error in case of mismatch.
      <%
         typed_nodes = [("Entity.Node", prop.owner)] + (
            [] if pred.is_variadic else [
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

      Node := Entity.Node;

      % if pred.is_variadic:
      <% expected_type = arr_arg.type.element_type.element_type %>
      for I in 2 .. Entities'Last loop
         % if has_multiple_concrete_nodes and not expected_type.is_root_node:
            if Entities (I).Node /= null
               and then Entities (I).Node.Kind not in
                  ${expected_type.ada_kind_range_name}
            then
               Raise_Property_Exception
                 (Entities (I).Node,
                  Property_Error'Identity,
                  "mismatching node type");
            end if;
         % endif
         Args.Items (I - 1) := (Entities (I).Node, Entities (I).Info);
      end loop;
      % endif

      ## Pass the "prefix" node (the one that owns the property to call) using
      ## the conventional bare node/entity_info arguments. Pass the other node
      ## arguments as entities directly.
      <%
         args = ['Node']
         if pred.is_variadic:
            args.append("Args")
         else:
            args.extend([
               f"(Node => Entities ({i + 1}).Node,"
               f" Info => Entities ({i + 1}).Info)"
               for i, formal_type in enumerated_arg_types
            ])
         args.extend(
            [f"{pa.name} => Self.Field_{pa.index}" for pa in pred.partial_args]
         )
         if prop.uses_entity_info:
            args.append(f"{prop.entity_info_name} => Entity.Info")
         args_fmt = '({})'.format(', '.join(args)) if args else ''
      %>

      Ret := ${prop.names.codegen} ${args_fmt};

      % if pred.is_variadic:
      Dec_Ref (Args);
      % endif;

      return Ret;
   % if pred.is_variadic:
   exception
      when Exc : Property_Error =>
         pragma Unreferenced (Exc);
         Dec_Ref (Args);
         raise;
   % endif
   end Call;

   % if prop.predicate_error is not None:
   ------------
   -- Failed --
   ------------

   ${failed_profile(pred)}
   is
      <%
         pred_error = prop.predicate_error
         args = pred_error.args_for_arity(pred.arity)
      %>
      Args : Internal_Entity_Array_Access :=
         Create_Internal_Entity_Array (${len(args)});

      Contexts : Internal_Logic_Context_Array_Access :=
         Create_Internal_Logic_Context_Array (Ctxs'Length);

      Diag : constant Internal_Solver_Diagnostic :=
        (Message_Template => Create_String ("${pred_error.template}"),
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

   overriding function Image (Self : ${pred.type_name}) return String is
   begin
      return ${ascii_repr(prop.qualname)};
   end Image;

   % if pred.has_refcounted_args:
      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Self : in out ${pred.type_name}) is
      begin
         % for pa in pred.partial_args:
            % if pa.type.is_refcounted:
               Dec_Ref (Self.Field_${pa.index});
            % endif
         % endfor
         null;
      end Destroy;
   % endif

</%def>
