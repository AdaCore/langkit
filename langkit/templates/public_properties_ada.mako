## vim: filetype=makoada

<%namespace name="helpers" file="helpers.mako" />

<%def name="public_prototype(property)">
  (${property.self_arg_name} : ${property.struct.entity.api_name}'Class
   % for arg in property.arguments:
      ; ${arg.name} : ${arg.type.api_name}

      ## Make entity arguments class-wide so that 1) these property wrappers
      ## are not primitives and 2) we can give them default values.
      ${"'Class" if arg.type.is_entity_type else ''}

      % if arg.default_value is not None:
         := ${arg.default_value.render_public_ada_constant()}
      % endif
   % endfor
  ) return ${(property.type.api_name)}
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
         actuals = ['{} ({}.Node)'.format(property.struct.name, self_arg)]

         # Build the list of arguments to pass to the property. Unwrap
         # arguments if needed.

         for arg in property.arguments:
            if arg.type.is_entity_type:
               actual = (
                  '({type} ({name}.Node), Convert ({name}.E_Info))'.format(
                     type=arg.type.el_type.name,
                     name=arg.name
               ))

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
                        "      ({typ} ({arg} (I).Node),"
                        "       Convert ({arg} (I).E_Info));".format(
                            actual=actual,
                            arg=arg.name,
                            typ=arg.type.element_type.el_type.name,
                        ),
                        "end loop;",
                    ])
                else:
                    unwrap_code.append('{}.Items := {};'.format(actual,
                                                                arg.name))

            else:
                actual = str(arg.name)

            actuals.append(actual)

         if property.uses_entity_info:
             actuals.append('E_Info => Convert ({}.E_Info)'.format(self_arg))

         ## Wrap the result, if needed

         if property.type.is_entity_type:
             wrapped_result = (
                 '({} (Property_Result.El),'
                 ' Convert (Property_Result.Info))'.format(
                     root_node_type_name
                 )
             )

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
                     "      Result (I) :=",
                     "         ({} (Item.El), Convert (Item.Info));".format(
                         root_node_type_name
                     ),
                     "   end;",
                     "end loop;",
                 ])
             else:
                 wrapped_result = 'Property_Result.Items'
             wrap_code.append('Dec_Ref (Property_Result);')

         else:
            wrapped_result = 'Property_Result'
      %>

      % for name, typ in local_vars:
         ${name} : ${typ};
      % endfor
      Property_Result : ${property.type.name};
   begin
      % for chunk in unwrap_code:
         ${chunk}
      % endfor

      ## Call the property
      Property_Result := ${property.name}
         ${'({})'.format(', '.join(actuals)) if actuals else ''};

      % if wrap_code:
         return Result : ${property.type.api_name} := ${wrapped_result}
         do
            % for chunk in wrap_code:
               ${chunk}
            % endfor
         end return;
      % else:
         return ${wrapped_result};
      % endif
   end;
</%def>
