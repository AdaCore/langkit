<%namespace name="exts" file="/extensions.mako" />
<%
base = f"com.adacore.{ctx.lib_name.lower}.{ctx.lib_name.camel}"
api = java_api
%>

[

    ${exts.include_extension(ctx.ext("native_image_api", "reflect_config"))}

    % for astnode in ctx.astnode_types:
        % if astnode != T.root_node:
    {
        "name" : "${base}$${api.wrapping_type(astnode)}",
        "allDeclaredConstructors" : true,
        "allPublicConstructors" : true,
        "allDeclaredMethods" : true,
        "allPublicMethods" : true
    },
        % endif
    % endfor

    {
        "name" : "${base}$${api.wrapping_type(T.root_node)}",
        "allDeclaredConstructors" : true,
        "allPublicConstructors" : true,
        "allDeclaredMethods" : true,
        "allPublicMethods" : true
    }

]
