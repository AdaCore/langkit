## vim: filetype=makoada

<%def name="accessor_profile(field)">
   <% accessor_name = capi.get_name(field.accessor_basename) %>

   function ${accessor_name}
     (Node    : ${node_type};

      % for arg in field.arguments:
         ${arg.name} : ${'access constant ' if arg.type.is_ada_record else ''}
                       ${arg.type.c_type(capi).name};
      % endfor

      ${field.entity_info_name} : access constant
         ${T.entity_info.c_type(capi).name};
      Value_P : access ${field.type.c_type(capi).name}) return int
</%def>

<%def name="accessor_decl(field)">
   <% accessor_name = capi.get_name(field.accessor_basename) %>

   ${accessor_profile(field)}
      with Export        => True,
           Convention    => C,
           External_name => "${accessor_name}";
   ${ada_doc(field, 3, lang='c')}
</%def>


<%def name="accessor_body(field)">

   <%
      struct = field.struct
      accessor_name = capi.get_name(field.accessor_basename)

      def simple_wrapping(t):
         return (t.is_lexical_env_type
                 or t.is_equation_type
                 or t.is_logic_var_type
                 or is_env_rebindings_type(t))
   %>

   ${accessor_profile(field)}
   is
      % if not field.is_property or not field.uses_entity_info:
         pragma Unreferenced (${field.entity_info_name});
      % endif

      Unwrapped_Node : constant ${root_node_type_name} := Unwrap (Node);
      ## For each input argument, convert the C-level value into an Ada-level
      ## one.
      % for arg in field.arguments:
         <%
            arg_ref = arg.name

            if arg.type.is_ada_record:
               arg_ref = '{}.all'.format(arg_ref)
         %>
         Unwrapped_${arg.name} : constant ${arg.type.name} :=
            % if arg.type.is_enum_type:
               ${field.type} (${arg_ref})
            % elif arg.type.is_bool_type:
               ${arg_ref} /= 0
            % elif arg.type.is_long_type:
               Integer (${arg_ref})
            % elif arg.type.is_analysis_unit_type:
               Unwrap (${arg_ref})
            % elif arg.type.is_analysis_unit_kind:
               Unit_Kind'Val (${arg_ref})
            % elif arg.type.is_ast_node:
               ${arg.type.name} (Unwrap (${arg_ref}))
            % elif arg.type.is_token_type:
               Token (Node, Token_Index ({arg_ref}.Index))
            % elif arg.type.is_symbol_type:
               Unwrap (Unwrapped_Node.Unit, ${arg_ref})
            % elif simple_wrapping(arg.type):
               Unwrap (${arg_ref})
            % else:
               ${arg_ref}
            % endif
         ;
      % endfor
   begin
      Clear_Last_Exception;

      % for arg in field.arguments:
         % if arg.type.is_token_type:
            if Unwrap (${arg_ref}).Unit /= Unwrapped_Node.Unit then
               raise Constraint_Error with
                 ("The input token does not belong to the same unit as"
                  & " the input node");
            end if;
         % endif
      % endfor

      if Unwrapped_Node.all in ${struct.value_type_name()}'Class then
         declare
            Typed_Node : constant ${struct.name} :=
               ${struct.name} (Unwrapped_Node);
         begin
             <%
               field_access = 'Typed_Node.{}'.format(field.name)

               actuals = ['{0.name} => Unwrapped_{0.name}'.format(a)
                          for a in field.arguments]
               if field.is_property and field.uses_entity_info:
                   actuals.append('{arg} => {arg}.all'.format(
                       arg=field.entity_info_name
                   ))
               field_access = '{}{}'.format(
                   field_access,
                   ' ({})'.format(', '.join(actuals))
                   if actuals else ''
               )
               if not field.is_property:
                  field_access = field.type.extract_from_storage_expr(
                     'Unwrapped_Node', field_access
                  )
             %>
             Value_P.all :=
                % if field.type.is_enum_type:
                    ${field.type.c_type(capi).name}
                      (${field.type.name}'Pos (${field_access}))
                % elif field.type.is_bool_type:
                    ${bool_type} (Boolean'Pos (${field_access}))
                % elif field.type.is_long_type:
                    int (${field_access})
                % elif field.type.is_analysis_unit_type:
                    Wrap (${field_access})
                % elif field.type.is_analysis_unit_kind:
                    Unit_Kind'Pos (${field_access})
                % elif field.type.is_ast_node:
                    Wrap (${root_node_type_name} (${field_access}))
                % elif field.type.is_token_type:
                    Wrap (${field_access})
                % elif field.type.is_symbol_type:
                    Wrap (${field_access})
                % elif simple_wrapping(field.type):
                    Wrap (${field_access})
                % else:
                    ${field_access}
                % endif
             ;
             return 1;
         exception
            when Exc : Property_Error =>
               ## If we reach this handler, it means the expression failed at
               ## some point because of a safety check. Tell the user about
               ## it.
               Set_Last_Exception (Exc, Is_Fatal => False);
               return 0;
         end;
      else
         return 0;
      end if;
   exception
      when Exc : others =>
         Set_Last_Exception (Exc);
         return 0;
   end ${accessor_name};

</%def>
