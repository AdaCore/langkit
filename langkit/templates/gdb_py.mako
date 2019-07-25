## vim: filetype=makopython

import sys
sys.path.append(${repr(langkit_path)})

<%
    astnode_names = [node.kwless_raw_name.camel_with_underscores
                     for node in ctx.astnode_types]
    astnode_kinds = {kind: node.kwless_raw_name.camel_with_underscores
                     for node, kind in ctx.node_kind_constants.items()}
%>

import langkit.gdb
langkit.gdb.setup(
    lib_name=${repr(lib_name)},
    astnode_names=${repr(astnode_names)},
    astnode_kinds=${repr(astnode_kinds)},
    prefix=${repr(prefix)}
)
