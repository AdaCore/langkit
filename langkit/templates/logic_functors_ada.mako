## vim: filetype=makoada

<%def name="create_profile(functor)">
   <%
      args = []
      if not functor.is_converter:
         args.append(("N", "Positive"))
      for pa in functor.partial_args:
         args.append((pa.name, pa.type.name))
   %>
   function ${functor.constructor_name}
   % if args:
   (
      % for arg_name, arg_type in args:
         ${arg_name} : ${arg_type}${"" if loop.last else ";"}
      % endfor
   )
   % endif
     return ${functor.type_name}
</%def>

<%def name="subp_profile(functor)">
   overriding function ${functor.subp_name} (
      Self : ${functor.type_name};
      % if functor.arity == 1:
         From : ${T.entity.name}
      % else:
         Vals : Entity_Vars.Value_Array
      % endif
   ) return ${T.entity.name}
</%def>

<%def name="decl(functor)">
   type ${functor.type_name} is new ${f"Solver_Ifc.{functor.base_type}"} with
   % if functor.partial_args:
      record
         % for pa in functor.partial_args:
            ${pa.name} : ${pa.type.name};
         % endfor
      end record
   % else:
      null record
   % endif
   with First_Controlling_Parameter;

   ${subp_profile(functor)} with Inline;
   overriding function Image (Self : ${functor.type_name}) return String;

   % if functor.has_refcounted_args:
      overriding procedure Destroy (Self : in out ${functor.type_name});
   % endif

   ${create_profile(functor)};
</%def>

<%def name="body(functor)">
   <%
      prop = functor.prop
      error_name = "conv_prop" if functor.is_converter else "comb_prop"
      has_multiple_concrete_nodes = len(T.root_node.concrete_subclasses) > 1
   %>

   ${create_profile(functor)} is
   begin
      <%
         components = []
         if not functor.is_converter:
            components.append(f"N => N")
         components += [
            "Cache_Set => False",
            "Cache_Key => <>",
            "Cache_Value => <>",
            "Ref_Count => 1",
         ]
      %>
      % for pa in functor.partial_args:
         % if pa.type.is_refcounted:
            Inc_Ref (${pa.name});
         % endif
         <% components.append(f"{pa.name} => {pa.name}") %>
      % endfor
      return ${functor.type_name}'(${", ".join(components)});
   end ${functor.constructor_name};

   ${subp_profile(functor)} is
      % if not functor.partial_args:
         pragma Unreferenced (Self);
      % endif

      ## If there is no From argument, create a local variable so that code
      ## generation below can always refer to the controlling argument as
      ## "From".
      % if not functor.is_converter:
         From : constant ${T.entity.name} := Vals (1);
      % endif

      % if functor.is_variadic:
         <% arr_arg = prop.natural_arguments[0] %>
         Args : ${arr_arg.type.name} :=
           ${arr_arg.type.constructor_name} (Vals'Length - 1);
      % endif

      <%
         # Range of "Vals" indexes for the entity arguments to pass to the
         # property in addition to "Self".
         #
         # Index 1 is for "Self", so the other entity arguments start at index
         # 2. For variadic combiners it is not supported to have additional
         # entities passed through logic vars so we use an empty range.
         extra_entity_args_range = (
             [] if functor.is_variadic else range(2, functor.arity + 1)
         )

         # List of all entity arguments ("Self" included) to pass to the
         # property, plus their expected types.
         typed_entity_args = [
            ("From", prop.owner),
         ] + [
            (f"Vals ({i})", arg.type.element_type)
            for i, arg in zip(
               extra_entity_args_range, prop.natural_arguments
            )
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

      % if functor.is_variadic:
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

         if functor.is_variadic:
            args.append(f"{arr_arg.name} => Args")
         else:
            # Pass other entity arguments. Pass an aggregate as the property
            # may take a non-root entity type, while "Vals" contains only root
            # entities.
            for i, vals_index in enumerate(extra_entity_args_range):
               args.append(
                  f"{prop.natural_arguments[i].name} =>"
                  f" (Node => Vals ({vals_index}).Node,"
                  f"  Info => Vals ({vals_index}).Info)"
               )

         # Pass all the required dynamic variables from the closure
         for pa in functor.partial_args:
            args.append(
               f"{pa.name} => Self.{pa.name}"
            )

         # Pass the entity info argument, if the property takes one
         if prop.uses_entity_info:
            args.append(f"{prop.entity_info_name} => From.Info")
      %>
      Ret := ${prop.names.codegen} (${", ".join(args)});

      % if functor.is_variadic:
      Dec_Ref (Args);
      % endif;

      return (Node => Ret.Node, Info => Ret.Info);
   % if functor.is_variadic:
   exception
      when Exc : Property_Error =>
         pragma Unreferenced (Exc);
         Dec_Ref (Args);
         raise;
   % endif
   end ${functor.subp_name};

   -----------
   -- Image --
   -----------

   overriding function Image (Self : ${functor.type_name}) return String is
   begin
      return (${ascii_repr(prop.qualname)});
   end Image;

   % if functor.has_refcounted_args:
      -------------
      -- Destroy --
      -------------

      overriding procedure Destroy (Self : in out ${functor.type_name}) is
      begin
         % for pa in functor.partial_args:
            % if pa.type.is_refcounted:
               Dec_Ref (Self.${pa.name});
            % endif
         % endfor
         null;
      end Destroy;
   % endif

</%def>
