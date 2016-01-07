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
      if N.all in ${astnode.name()}_Type'Class then
         declare
            Typed_Node : constant ${astnode.name()} := ${astnode.name()} (N);
         begin
             % if is_enum(field.type):
                 Value_P.all := ${field.type.c_type(capi).name}
                   (${field.type.name()}'Pos (Typed_Node.${field.name}));
             % elif is_bool(field.type):
                 Value_P.all := int (Boolean'Pos (Typed_Node.${field.name}));
             % elif is_ast_node(field.type):
                 Value_P.all :=
                   Wrap (${root_node_type_name} (Typed_Node.${field.name}));
             % elif is_token_type(field.type):
                 Value_P.all := Wrap (Typed_Node.${field.name}'Access);
             % else:
                 Value_P.all := Typed_Node.${field.name};
             % endif
             return 1;
         exception
            when Property_Error =>
               ## If we reach this handler, it means the expression failed at
               ## some point because of a safety check. Tell the user about
               ## it.
               return 0;

            ## TODO: if code generation is buggy, we may have other exceptions
            ## here. We should find a way to propagate them cleanly through
            ## bindings so that, for instance, we do not just crash in Python
            ## but have a proper stack trace.
         end;
      else
         return 0;
      end if;
   end ${accessor_name};

</%def>
