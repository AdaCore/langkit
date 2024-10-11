"""
Check that GDB helpers work as expected.

This is meant to be "the" big test that checks all aspects of GDB helpers:
Langkit-defined commands, pretty-printers, etc. To achieve this in a simple
way (i.e. not creating a big realistic language spec) we just create dummy
properties that implement the control-flow we want, or that deal with the types
for which we need to check pretty-printers: our goal here is just to cover GDB
helpers features.
"""

import subprocess
import sys

from utils import build_and_run


# Build the generated library and the Ada test program
build_and_run(
    lkt_file="test.lkt",
    gpr_mains=["main.adb"],
    unparse_script=[],
    types_from_lkt=True,
)

# Run the test program under GDB to check the helpers. We keep this part in
# separate scripts to make it convenient, for debugging purposes, to run these
# checks without re-building the library/program.
for script in [
    "check_printers.py",
    "check_control_flow.py",
    "check_recursive_cf.py",
    "check_state.py",
]:
    subprocess.check_call([sys.executable, script])

print("Done")
