"""
GDB helpers to debug generated libraries.
"""

from __future__ import absolute_import, division, print_function

import gdb

from langkit.gdb import commands, functions, printers
from langkit.gdb.context import Context


setup_done = False
gdb_printers = None
global_context = None


def get_current_gdb_context():
    """
    This function is meant to be called from a running gdb instance. It will
    return the relevant Context instance depending on the code being executed.

    :rtype: Context
    """
    global global_context
    return global_context


def setup(lib_name, astnode_names, prefix):
    """
    Register helpers in GDB internals. This should be run when the generated
    library is actually loaded in GDB.
    """
    global setup_done, gdb_printers, global_context
    setup_done = True

    context = Context(lib_name, astnode_names, prefix)
    global_context = context

    gdb_printers = printers.GDBPrettyPrinters(context)
    for printer in [
        printers.AnalysisUnitPrinter,
        printers.ASTNodePrinter,
        printers.LexicalEnvPrinter,
        printers.EnvGetterPrinter,
        printers.ReferencedEnvPrinter,
        printers.EntityPrinter,
        printers.ArrayPrettyPrinter,
        printers.LangkitVectorPrinter,
    ]:
        gdb_printers.append(printer)

    for objfile in gdb.objfiles():
        handle_new_objfile(objfile, lib_name)
    gdb.events.new_objfile.connect(
        lambda event: handle_new_objfile(event.new_objfile, lib_name)
    )

    for cmd_cls in [
        commands.StateCommand,
        commands.BreakCommand,
        commands.NextCommand,
        commands.OutCommand,
    ]:
        cmd_cls(context)

    functions.Match(context)


def handle_new_objfile(objfile, lib_name):
    # Registers our printers only for the objfile that contains the generated
    # library.
    version_symbol = gdb.lookup_global_symbol('{}__version'.format(lib_name))
    if version_symbol is None or version_symbol.symtab.objfile != objfile:
        return

    objfile.pretty_printers.append(gdb_printers)
