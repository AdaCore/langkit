## vim: filetype=makocpp


<%def name="accessor_decl(field)">

   <% accessor_name = capi.get_name(field.accessor_basename) %>

   ${c_doc(field)}
   extern int
   ${accessor_name}(${node_type} node,
                    ${field.type.c_type(capi).name} *value_p);

</%def>
