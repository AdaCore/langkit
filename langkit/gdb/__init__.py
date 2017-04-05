"""
GDB helpers to debug generated libraries.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import gdb

from langkit.gdb import printers
from langkit.gdb.utils import system_address, ptr_to_int
from langkit.names import Name


setup_done = False
gdb_printers = None


def setup(lib_name, astnode_names):
    """
    Register helpers in GDB internals. This should be run when the generated
    library is actually loaded in GDB.
    """
    global setup_done, gdb_printers
    setup_done = True

    gdb_printers = printers.GDBPrettyPrinters(lib_name, [])
    for printer in [
        printers.GDBSubprinter(
            printers.ASTNodePrinter,
            astnode_struct_names=astnode_struct_names(lib_name, astnode_names),
            tags_mapping=tags_mapping(lib_name, astnode_names),
        ),
    ]:
        gdb_printers.append(printer)

    for objfile in gdb.objfiles():
        handle_new_objfile(objfile, lib_name)
    gdb.events.new_objfile.connect(
        lambda event: handle_new_objfile(event.new_objfile, lib_name)
    )


def astnode_struct_names(lib_name, astnode_names):
    """
    Turn the given set of ASTNode subclass names (lowercase) into a set of
    ASTNode record names, as GDB will see them.
    """
    return {
        '{}__analysis__{}_type'.format(lib_name, name)
        for name in astnode_names
    }


def tags_mapping(lib_name, astnode_names):
    """
    Build a mapping: address (int) -> AST node pretty name.

    Each Address is the address of the AST node type tag.
    """
    # The symbols we are looking up here do not correspond exactly to the _tag
    # field we see in tagged records: we need to add an offset to them. This
    # offset is 4 times the size of a pointer. See GNAT's a-tags.ads file for
    # more details.
    tag_offset = 4 * system_address.sizeof

    return {
        ptr_to_int(gdb.parse_and_eval(
            '<{}__analysis__{}_typeT>'.format(lib_name, name)
        ).address) + tag_offset: Name.from_lower(name).camel
        for name in astnode_names
    }


def handle_new_objfile(objfile, lib_name):
    # Registers our printers only for the objfile that contains the generated
    # library.
    version_symbol = gdb.lookup_global_symbol('{}__version'.format(lib_name))
    if version_symbol is None or version_symbol.symtab.objfile != objfile:
        return

    objfile.pretty_printers.append(gdb_printers)
