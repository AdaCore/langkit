from typing import Dict, List

import gdb

import langkit.gdb
from langkit.gdb import commands, functions, printers
from langkit.gdb.context import Context


def get_current_gdb_context() -> Context:
    """
    This function is meant to be called from a running gdb instance. It will
    return the relevant Context instance depending on the code being executed.

    :rtype: Context
    """
    assert langkit.gdb.global_context
    return langkit.gdb.global_context


def setup(lib_name: str,
          astnode_names: List[str],
          astnode_kinds: Dict[int, str],
          prefix: str) -> None:
    """
    Register helpers in GDB internals. This should be run when the generated
    library is actually loaded in GDB.
    """
    langkit.gdb.setup_done = True

    context = Context(lib_name, astnode_names, astnode_kinds, prefix)
    langkit.gdb.global_context = context

    langkit.gdb.gdb_printers = printers.GDBPrettyPrinters(context)
    for printer in [
        printers.AnalysisUnitPrinter,
        printers.ASTNodePrinter,
        printers.EnvNamePrinter,
        printers.LexicalEnvPrinter,
        printers.EnvGetterPrinter,
        printers.ReferencedEnvPrinter,
        printers.EntityPrinter,
        printers.ArrayPrettyPrinter,
        printers.LangkitVectorPrinter,
        printers.RebindingsPrinter,
        printers.TokenReferencePrinter,
    ]:
        langkit.gdb.gdb_printers.append(printer)

    for objfile in gdb.objfiles():
        handle_new_objfile(objfile, lib_name, reparse_debug_info=False)
    gdb.events.new_objfile.connect(
        lambda event: handle_new_objfile(event.new_objfile, lib_name)
    )

    for cmd_cls in [
        commands.StateCommand,
        commands.BreakCommand,
        commands.NextCommand,
        commands.OutCommand,
        commands.StepInsideCommand,
    ]:
        cmd_cls(context)

    functions.Match(context)


def handle_new_objfile(objfile: gdb.Objfile,
                       lib_name: str,
                       reparse_debug_info: bool = True) -> None:
    # Registers our printers only for the objfile that contains the generated
    # library.
    version_symbol = gdb.lookup_global_symbol('{}__version'.format(lib_name))
    if version_symbol is None or version_symbol.symtab.objfile != objfile:
        return

    objfile.pretty_printers.append(langkit.gdb.gdb_printers)

    if reparse_debug_info:
        get_current_gdb_context().reparse_debug_info()
