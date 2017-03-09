## vim: filetype=makocpp


<%def name="accessor_decl(field)">

<% accessor_name = capi.get_name(field.accessor_basename) %>

${c_doc(field)}
extern int ${accessor_name}(
    ${node_type} node,

    % for arg in field.explicit_arguments:
        ${arg.type.c_type(capi).name} ${arg.name},
    % endfor

    ${field.c_type_or_error(capi).name} *value_p
);

</%def>
