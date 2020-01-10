## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

## In the following helper, use string formatting instead of regular templating
## to make generated sources easier to read.
<%def name="public_prototype(property)"><%
      args = [(property.self_arg_name,
               "{}'Class".format(property.struct.entity.api_name),
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
</%def>

<%def name="body(property)">
   function ${property.api_name}
${public_prototype(property)} is
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
            := ${property.qual_impl_name}
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
