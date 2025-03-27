"""
Checks for the "state" command.
"""

from helpers import break_dsl, run_continue, start_gdb


gdb = start_gdb("printers")

# Check that multi-line values are correctly indented. To check this, print a
# struct value and enable the pretty output (i.e. newlines).
break_dsl("test_struct")
run_continue()
gdb.test("set print pretty on", "")
gdb.test(
    "foost result",
    ("result = (" "\n  a => 1, " "\n  b => 11" "\n)"),
    quotemeta=False,
)
