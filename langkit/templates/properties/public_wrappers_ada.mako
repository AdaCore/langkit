## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

<%def name="public_prototype(property)">
  (${property.self_arg_name} : ${property.struct.entity.api_name}'Class
   % for arg in property.arguments:
      ; ${arg.name} : ${arg.public_type.api_name}

      ## Make entity arguments class-wide so that 1) these property wrappers
      ## are not primitives and 2) we can give them default values. Likewise
      ## for analysis units.
      ${"'Class" if (arg.public_type.is_entity_type or
                     arg.public_type.is_analysis_unit_type) else ''}

      % if arg.default_value is not None:
         := ${arg.public_default_value.render_public_ada_constant()}
      % endif
   % endfor
  ) return ${(property.public_type.api_name)}
</%def>

<%def name="decl(property)">
   function ${property.api_name} ${public_prototype(property)};
   ${ada_doc(property, 3)}
</%def>

<%def name="body(property)">
   function ${property.api_name} ${public_prototype(property)} is
      <%
         self_arg = property.self_arg_name
         context_expr = '{}.Internal.Node.Unit.Context'.format(self_arg)
      %>
   begin
      Check_Safety_Net (${self_arg}.Safety_Net);

      declare
         ## Convert property arguments to internal types
         % for arg in property.arguments:
            Internal_Arg_${arg.name} :
               ${'' if arg.type.is_refcounted else 'constant'} ${arg.type.name}
               := ${arg.type.to_internal_expr(str(arg.name), context_expr)};
         % endfor

         ## Call the property
         <%
            actuals = [
               '{} ({}.Internal.Node)'.format(T.root_node.name, self_arg)
            ] + ['Internal_Arg_{}'.format(arg.name)
                 for arg in property.arguments]
            if property.uses_entity_info:
                actuals.append('E_Info => {}.Internal.Info'.format(self_arg))
         %>
         Property_Result
            : ${'' if property.type.is_refcounted else 'constant'}
              ${property.type.name}
            := ${property.name}
               ${'({})'.format(', '.join(actuals)) if actuals else ''};
      begin
         ## Compute the list of variables that need to be dec-ref'd before
         ## returning.
         <%
            to_decref = ['Internal_Arg_{}'.format(arg.name)
                         for arg in property.arguments
                         if arg.type.is_refcounted]
            if property.type.is_refcounted:
               to_decref.append('Property_Result')

            result_expr = property.type.to_public_expr('Property_Result')
         %>

         ## Finally, return after the required dec-ref
         % if to_decref:
            return Result : constant ${property.public_type.api_name} :=
               ${result_expr}
            do
               % for var in to_decref:
                  Dec_Ref (${var});
               % endfor
            end return;
         % else:
            return ${result_expr};
         % endif
      end;
   end;
</%def>
