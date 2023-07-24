## vim: filetype=makopython

"""
This script is meant to be sourced inside GDB when debugging a program that
uses ${lib_name}. It installs Langkit's "GDB helpers": pretty-printers and
custom commands that are convenient when debugging ${lib_name}. Note that GDB
helpers require the Langkit Python library.
"""

import sys


sys.path.append(${repr(langkit_path)})


try:
    import langkit.gdb
except ImportError as exc:
    print(
        f"{__file__}: Cannot import the 'langkit.gdb' Python package: langkit"
        " GDB helpers require it",
        file=sys.stderr,
    )
else:
    <%
        astnode_names = [node.kwless_raw_name.camel_with_underscores
                         for node in ctx.astnode_types]
        astnode_kinds = {kind: node.kwless_raw_name.camel_with_underscores
                         for node, kind in ctx.node_kind_constants.items()}
    %>
    langkit.gdb.setup(
        lib_name=${repr(lib_name)},
        astnode_names=${repr(astnode_names)},
        astnode_kinds=${repr(astnode_kinds)},
        prefix=${repr(prefix)}
    )
