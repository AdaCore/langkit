"""
GDB helpers to debug generated libraries.
"""


try:
    import gdb
except ImportError:
    gdb = None


setup_done = False
gdb_printers = None
global_context = None


# Import actual GDB helpers only if running inside GDB
if gdb:
    # pyflakes off
    from langkit.gdb import commands, functions, printers
    from langkit.gdb.context import Context
    from langkit.gdb.setup import get_current_gdb_context, setup
    # pyflakes on
