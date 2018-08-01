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
   function ${property.name} ${public_prototype(property)};
   ${ada_doc(property, 3)}
</%def>

<%def name="body(property)">
   function ${property.name} ${public_prototype(property)} is
      <%
         self_arg = property.self_arg_name

         local_vars = []
         unwrap_code = []
         wrap_code = []
         actuals = ['{} ({}.Internal.El)'
                    .format(property.struct.name, self_arg)]

         # Build the list of arguments to pass to the property. Unwrap
         # arguments if needed.

         for arg in property.arguments:
            if arg.type.is_entity_type:
               actual = (
                  '({type} ({name}.Internal.El), {name}.Internal.Info)'
                  .format(type=arg.type.el_type.name, name=arg.name)
               )

            elif arg.type.is_ast_node:
               actual = '{type} ({name}.Internal.El)'.format(
                  type=arg.type.name,
                  name=arg.name
               )

            elif arg.type.is_array_type:
                # We need to allocate our special record type to pass it to the
                # property.
                actual = 'Unwrapped_{}'.format(arg.name)
                local_vars.append((actual, arg.type.name))
                unwrap_code.append("{} := new {} ({}'Length);".format(
                    actual, arg.type.pointed, arg.name
                ))
                unwrap_code.append('Dec_Ref ({});'.format(actual))

                if arg.type.element_type.is_entity_type:
                    unwrap_code.extend([
                        "for I in {arg}'Range loop"
                        "   {actual}.Items (I) :="
                        "      ({typ} ({arg} (I).Internal.El),"
                        "       {arg} (I).Internal.Info);".format(
                            actual=actual,
                            arg=arg.name,
                            typ=arg.type.element_type.el_type.name,
                        ),
                        "end loop;",
                    ])
                else:
                    unwrap_code.append('{}.Items := {};'.format(actual,
                                                                arg.name))

            elif arg.type.is_big_integer_type:
                actual = 'Create ({})'.format(arg.name)

            elif arg.type.is_analysis_unit_type:
                actual = 'Unwrap_Unit ({})'.format(arg.name)

            else:
                actual = str(arg.name)

            actuals.append(actual)

         if property.uses_entity_info:
             actuals.append('E_Info => {}.Internal.Info'.format(self_arg))

         ## Wrap the result, if needed

         def wrap_node(node_expr, einfo_expr, node_type):
             result = 'Wrap_Node ({}, {})'.format(node_expr, einfo_expr)
             if not node_type.is_root_node:
                 result = '{}.As_{}'.format(result, node_type.entity.api_name)
             return result

         if property.type.is_entity_type:
             wrapped_result = wrap_node('Property_Result.El',
                                        'Property_Result.Info',
                                        property.type.astnode)

         elif property.type.is_ast_node:
             wrapped_result = wrap_node('Property_Result', 'No_Entity_Info',
                                        property.type)

         elif property.type.is_array_type:
             if property.type.element_type.is_entity_type:
                 entity_type = property.type.element_type.name
                 wrapped_result = ''
                 wrapped_result = '(1 .. Property_Result.N => <>)'
                 wrap_code.extend([
                     "for I in Result'Range loop",
                     "   declare",
                     "      Item : {} renames".format(entity_type),
                     "         Property_Result.Items (I);",
                     "   begin",
                     "      Result (I) := {};".format(wrap_node(
                         'Item.El', 'Item.Info',
                         property.type.element_type.astnode
                     )),
                     "   end;",
                     "end loop;",
                 ])
             else:
                 wrapped_result = '{} (Property_Result.Items)'.format(
                     property.type.api_name
                 )
             wrap_code.append('Dec_Ref (Property_Result);')

         elif property.type.is_big_integer_type:
             wrapped_result = None
             wrap_code.extend([
                 'Result.Set (Property_Result.Value);',
                 'Dec_Ref (Property_Result);'
             ])

         elif property.type.is_analysis_unit_type:
             wrapped_result = 'Wrap_Unit (Property_Result)'

         else:
             wrapped_result = 'Property_Result'
      %>

      % for name, typ in local_vars:
         ${name} : ${typ};
      % endfor
      Property_Result : ${property.type.name};
   begin
     Check_Safety_Net (${self_arg}.Safety_Net);

      % for chunk in unwrap_code:
         ${chunk}
      % endfor

      ## Call the property
      Property_Result := ${property.name}
         ${'({})'.format(', '.join(actuals)) if actuals else ''};

      % if wrap_code:
         return Result : ${property.public_type.api_name} ${(
            ':= {}'.format(wrapped_result)
            if wrapped_result else ''
         )} do
            % for chunk in wrap_code:
               ${chunk}
            % endfor
         end return;
      % else:
         return ${wrapped_result};
      % endif
   end;
</%def>
