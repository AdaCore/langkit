## vim: filetype=makoada

<%def name="accessor_decl(field)">

   <% accessor_name = capi.get_name(field.accessor_basename) %>

   function ${accessor_name}
     (Node    : ${node_type};
      Value_P : ${field.type.c_type(capi).name}_Ptr) return int
      with Export        => True,
           Convention    => C,
           External_name => "${accessor_name}";
   ${ada_doc(field, lang='c')}

</%def>


<%def name="accessor_body(field)">

   <%
      astnode = field.ast_node
      accessor_name = capi.get_name(field.accessor_basename)
   %>

   function ${accessor_name}
     (Node    : ${node_type};
      Value_P : ${field.type.c_type(capi).name}_Ptr) return int
   is
      N : constant ${root_node_type_name} := Unwrap (Node);
   begin
      Clear_Last_Exception;

      if N.all in ${astnode.name()}_Type'Class then
         declare
            Typed_Node : constant ${astnode.name()} := ${astnode.name()} (N);
         begin
             <%
               field_access = 'Typed_Node.{}'.format(field.name)
             %>
             Value_P.all :=
                % if is_enum(field.type):
                    ${field.type.c_type(capi).name}
                      (${field.type.name()}'Pos (${field_access}))
                % elif is_bool(field.type):
                    int (Boolean'Pos (${field_access}))
                % elif is_long(field.type):
                    int (${field_access})
                % elif is_ast_node(field.type):
                    Wrap (${root_node_type_name} (${field_access}))
                % elif is_token_type(field.type):
                    Wrap (${field_access}'Access)
                % else:
                    ${field_access}
                % endif
             ;
             return 1;
         exception
            when Property_Error =>
               ## If we reach this handler, it means the expression failed at
               ## some point because of a safety check. Tell the user about
               ## it.

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
