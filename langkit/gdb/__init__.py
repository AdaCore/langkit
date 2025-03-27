"""
GDB helpers to debug generated libraries.
"""

from __future__ import annotations

from typing import TYPE_CHECKING


has_gdb: bool = True
try:
    import gdb
except ImportError:
    has_gdb = False
else:
    del gdb

# Import actual GDB helpers only if running inside GDB
if has_gdb:
    # pyflakes off
    from langkit.gdb import commands, functions, printers
    from langkit.gdb.context import Context
    from langkit.gdb.setup import get_current_gdb_context, setup

    # pyflakes on

if TYPE_CHECKING:
    from langkit.gdb.printers import GDBPrettyPrinters


setup_done: bool = False
gdb_printers: GDBPrettyPrinters | None = None
global_context: Context | None = None
