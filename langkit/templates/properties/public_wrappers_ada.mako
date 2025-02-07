## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

## In the following helper, use string formatting instead of regular templating
## to make generated sources easier to read.
<%def name="public_prototype(property)"><%
      args = [(property.self_arg_name,
               "{}'Class".format(property.owner.entity.api_name),
               None)]
      for arg in property.arguments:
         # Make entity arguments class-wide so that 1) these property wrappers
         # are not primitives and 2) we can give them default values. Likewise
         # for analysis units.
         arg_type = arg.public_type.api_name
         if (
            arg.public_type.is_entity_type or
            arg.public_type.is_analysis_unit_type
         ):
               arg_type = "{}'Class".format(arg_type)

         default_val = (None if arg.default_value is None else
                        arg.public_default_value.render_public_ada_constant())

         args.append((arg.name, arg_type, default_val))
   %>${'     ({})'.format(';\n      '.join(
      '{} : {}{}'.format(arg_name, arg_type,
                         ' := {}'.format(default_val) if default_val else '')
      for arg_name, arg_type, default_val in args
   ))} return ${property.public_type.api_name}</%def>

<%def name="decl(property)">
   function ${property.api_name}
${public_prototype(property)};
   ${ada_doc(property, 3)}
   --% belongs-to: ${property.owner.entity.api_name}
</%def>

<%def name="body(property)">
   function ${property.api_name}
${public_prototype(property)} is
      <%
         self_arg = property.self_arg_name
         context_expr = '{}.Internal.Node.Unit.Context'.format(self_arg)

         def internal_arg_var(arg):
            """
            Name of the local variable that holds the internal value for the
            given argument.
            """
            return 'Internal_Arg_{}'.format(arg.name)

         refcounted_args = [arg for arg in property.arguments
                            if arg.type.is_refcounted]

         # If there is at least one argument (or the property result) whose
         # internal type is ref-counted , we need to have provision to dec-ref
         # corresponding values both on "regular" returns, and when an
         # exception is raised during property execution.
         needs_refcounting = refcounted_args or property.type.is_refcounted
      %>

      ## Declare variables to hold arguments and the result encoded with
      ## internal types.

      % for arg in property.arguments:
         ${internal_arg_var(arg)} : ${arg.type.name};
      % endfor
      Property_Result : ${property.type.name};

      % if needs_refcounting:
         procedure Free_Internal;
         --  Dec-ref all internal arguments and the property result, when
         --  applicable.

         -------------------
         -- Free_Internal --
         -------------------

         procedure Free_Internal is
         begin
            % for arg in refcounted_args:
               Dec_Ref (${internal_arg_var(arg)});
            % endfor
            % if property.type.is_refcounted:
               Dec_Ref (Property_Result);
            % endif
         end Free_Internal;
      % endif

   begin
      if ${self_arg}.Internal.Node = null then
         raise Precondition_Failure with "null node argument";
      end if;

      Check_Safety_Net (${self_arg});

      ## Convert property arguments to internal types
      % for arg in property.arguments:
         ${internal_arg_var(arg)} :=
            ${arg.type.to_internal_expr(str(arg.name), context_expr)};
      % endfor

      ## Call the property
      <%
         actuals = [
            '{} ({}.Internal.Node)'.format(T.root_node.name, self_arg)
         ] + [internal_arg_var(arg) for arg in property.arguments]
         if property.uses_entity_info:
             actuals.append('E_Info => {}.Internal.Info'.format(self_arg))

         result_expr = property.type.to_public_expr('Property_Result')
      %>
      Property_Result :=
         ${property.qual_impl_name}
            ${'({})'.format(', '.join(actuals)) if actuals else ''};

      ## Return its result, after conversion to public types
      return Result : ${property.public_type.api_name} := ${result_expr} do
         ## For properties that return bare nodes, automatically propagate
         ## the entity info from the node prefix.
         % if property.type.is_ast_node:
            Result := Wrap_Node
              (Property_Result, Node.Internal.Info)
              .As_${property.type.entity.api_name};
         % endif

         % if needs_refcounting:
            Free_Internal;
         % endif

         % if not property.type.is_ast_node and not needs_refcounting:
            null;
         % endif
      end return;

   ## Free resources when the property fails
   % if needs_refcounting:
      exception
         when ${ctx.property_exception_matcher} =>
            Free_Internal;
            raise;
   % endif
   end;
</%def>
