## vim: filetype=makoada

<%def name="accessor_decl(field)">

   <% accessor_name = capi.get_name(field.accessor_basename) %>

   function ${accessor_name}
     (Node    : ${node_type};

      % for arg in field.explicit_arguments:
         ${arg.name} : ${arg.type.c_type(capi).name};
      % endfor

      Value_P : ${field.type.c_type(capi).name}_Ptr) return int
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
         return (is_lexical_env(t)
                 or is_equation_type(t)
                 or is_logic_var_type(t))
   %>

   function ${accessor_name}
     (Node    : ${node_type};

      % for arg in field.explicit_arguments:
         ${arg.name} : ${arg.type.c_type(capi).name};
      % endfor

      Value_P : ${field.type.c_type(capi).name}_Ptr) return int
   is
      Unwrapped_Node : constant ${root_node_type_name} := Unwrap (Node);
      ## For each input argument, convert the C-level value into an Ada-level
      ## one.
      % for arg in field.explicit_arguments:
         Unwrapped_${arg.name} : constant ${arg.type.name()} :=
            % if is_enum(arg.type):
               ${field.type} (${arg.name})
            % elif is_bool(arg.type):
               ${arg.name} /= 0
            % elif is_long(arg.type):
               Integer (${arg.name})
            % elif is_analysis_unit(arg.type):
               Unwrap (${arg.name})
            % elif is_analysis_kind(arg.type):
               Unit_Kind'Val (${arg.name})
            % elif is_ast_node(arg.type):
               ${arg.type.name()} (Unwrap (${arg.name}))
            % elif is_token_type(arg.type):
               Token (Node, Token_Index ({arg.name}.Index))
            % elif is_symbol_type(arg.type):
               Text_To_Symbol (Unwrapped_Node.Unit, ${arg.name})
            % elif simple_wrapping(arg.type):
               Unwrap (${arg.name})
            % else:
               ${arg.name}
            % endif
         ;
      % endfor
   begin
      Clear_Last_Exception;

      % for arg in field.explicit_arguments:
         % if is_token_type(arg.type):
            if Unwrap (${arg.name}).Unit /= Unwrapped_Node.Unit then
               raise Constraint_Error with
                 ("The input token does not belong to the same unit as"
                  & " the input node");
            end if;
         % endif
      % endfor

      if Unwrapped_Node.all in ${struct.value_type_name()}'Class then
         declare
            Typed_Node : constant ${struct.name()} :=
               ${struct.name()} (Unwrapped_Node);
         begin
             <%
               field_access = 'Typed_Node.{}'.format(field.name)

               actuals = ', '.join('{0.name} => Unwrapped_{0.name}'.format(a)
                                   for a in field.explicit_arguments)
               field_access = '{}{}'.format(
                   field_access,
                   ' ({})'.format(actuals)
                   if actuals else ''
               )
               if not field.is_property:
                  field_access = field.type.extract_from_storage_expr(
                     'Unwrapped_Node', field_access
                  )
             %>
             Value_P.all :=
                % if is_enum(field.type):
                    ${field.type.c_type(capi).name}
                      (${field.type.name()}'Pos (${field_access}))
                % elif is_bool(field.type):
                    ${bool_type} (Boolean'Pos (${field_access}))
                % elif is_long(field.type):
                    int (${field_access})
                % elif is_analysis_unit(field.type):
                    Wrap (${field_access})
                % elif is_analysis_kind(field.type):
                    Unit_Kind'Pos (${field_access})
                % elif is_ast_node(field.type):
                    Wrap (${root_node_type_name} (${field_access}))
                % elif is_token_type(field.type):
                    Wrap (${field_access})
                % elif is_symbol_type(field.type):
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
