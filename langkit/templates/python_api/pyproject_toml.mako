## vim: filetype=makotoml

<% pyapi = ctx.python_api_settings %>

[build-system]
requires = ["setuptools"]
build-backend = "setuptools.build_meta"

[project]
name = ${repr(ctx.lib_name.camel)}
version = ${repr(pyapi.pep440_version)}

[tool.setuptools]
packages = [${repr(pyapi.module_name)}]

[tool.setuptools.package-data]
<%
    patterns = (
        ['*.{}'.format(ext) for ext in ('dll', 'so', 'so.*', 'dylib')]
        + ["py.typed"]
    )
%>
${repr(pyapi.module_name)} = [
    % for p in patterns:
    ${repr(p)},
    % endfor
]
