## vim: filetype=makoada

<%def name="accessor_profile(field)">
   <%
      accessor_name = capi.get_name(field.accessor_basename)
      entity_type = root_entity.c_type(capi).name
   %>

   function ${accessor_name}
     (Node : ${entity_type}_Ptr;

      % for arg in field.arguments:
         ${arg.name} : ${'access constant ' if arg.type.is_ada_record else ''}
                       ${arg.type.c_type(capi).name};
      % endfor

      Value_P : access ${(
         entity_type
         if field.type.is_ast_node else
         field.type.c_type(capi).name
      )}) return int
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
                 or t.is_env_rebindings_type)
   %>

   ${accessor_profile(field)}
   is
      Unwrapped_Node : constant ${root_node_type_name} := Node.El;
      ## For each input argument, convert the C-level value into an Ada-level
      ## one.
      % for arg in field.arguments:
         <%
            arg_ref = arg.name

            if arg.type.is_ada_record:
               arg_ref = '{}.all'.format(arg_ref)
         %>
         Unwrapped_${arg.name} : constant ${arg.type.name} :=
            % if arg.type.is_bool_type:
               ${arg_ref} /= 0
            % elif arg.type.is_long_type:
               Integer (${arg_ref})
            % elif arg.type.is_analysis_unit_type:
               Unwrap (${arg_ref})
            % elif arg.type.is_analysis_unit_kind:
               Unit_Kind'Val (${arg_ref})
            % elif arg.type.is_ast_node:
               ${arg.type.name} (Unwrap (${arg_ref}))
            % elif arg.type.is_entity_type:
               (if ${arg_ref}.El = null
                then ${arg.type.nullexpr}
                else (${arg.type.el_type.name} (${arg_ref}.El),
                      ${arg_ref}.Info))
            % elif arg.type.is_array and not arg.type.emit_c_type:
               Convert (${arg_ref})
            % elif arg.type.is_token_type:
               Token (Node, Token_Index ({arg_ref}.Index))
            % elif arg.type.is_symbol_type:
               Unwrap (Unwrapped_Node.Unit, ${arg_ref})
            % elif arg.type.is_big_integer_type:
               Unwrap (${arg_ref})
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
            <%
              # For properties, don't use the dot notation as it could conflict
              # with homonym fields.
              field_access = (str(field.name)
                              if field.is_property else
                              'Typed_Node.{}'.format(field.name))

              actuals = ['{0.name} => Unwrapped_{0.name}'.format(a)
                         for a in field.arguments]
              if field.is_property:
                  actuals.insert(0, 'Typed_Node')
              if field.is_property and field.uses_entity_info:
                  actuals.append('{} => Node.Info'.format(
                      field.entity_info_name
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

            Typed_Node : constant ${struct.name} :=
               ${struct.name} (Unwrapped_Node);
            Result     : ${field.type.name};
         begin
            --  Keep this assignment after the BEGIN keyword above so that the
            --  exception handler covers it.
            Result := ${field_access};

            Value_P.all :=
               % if field.type.is_bool_type:
                   ${bool_type} (Boolean'Pos (Result))
               % elif field.type.is_long_type:
                   int (Result)
               % elif field.type.is_analysis_unit_type:
                   Wrap (Result)
               % elif field.type.is_analysis_unit_kind:
                   Unit_Kind'Pos (Result)
               % elif field.type.is_ast_node:
                   (${root_node_type_name} (Result), Node.Info)
               % elif field.type.is_entity_type:
                  (${root_node_type_name} (Result.El), Result.Info)
               % elif field.type.is_array and not field.type.emit_c_type:
                  Convert (Result)
               % elif field.type.is_token_type:
                   Wrap (Result)
               % elif field.type.is_symbol_type:
                   Wrap (Result)
               % elif field.type.is_big_integer_type:
                   Wrap (Result)
               % elif simple_wrapping(field.type):
                   Wrap (Result)
               % else:
                   Result
               % endif
            ;

            % if field.type.is_big_integer_type:
               Dec_Ref (Result);
            % endif

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
