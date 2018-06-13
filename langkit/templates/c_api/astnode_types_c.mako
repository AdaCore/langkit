## vim: filetype=makocpp


<%def name="accessor_decl(field)">

<%
   accessor_name = capi.get_name(field.accessor_basename)
   entity_type = root_entity.c_type(capi).name
%>

${c_doc(field)}
extern int ${accessor_name}(
    ${entity_type} *node,

    % for arg in field.arguments:
        <% type_name = arg.public_type.c_type(capi).name %>
        ${('const {}*'.format(type_name)
           if arg.public_type.is_ada_record else type_name)}
        ${arg.name},
    % endfor

    ${field.c_type_or_error(capi).name} *value_p
);

</%def>
