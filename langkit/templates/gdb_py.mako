## vim: filetype=makopython

import sys
sys.path.append(${repr(langkit_path)})

<%
    astnode_names = [node.kwless_raw_name.camel_with_underscores
                     for node in ctx.astnode_types]
%>

import langkit.gdb
langkit.gdb.setup(
    lib_name=${repr(lib_name)},
    astnode_names=${repr(astnode_names)},
    prefix=${repr(prefix)}
)
