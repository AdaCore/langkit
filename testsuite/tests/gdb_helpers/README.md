Tests to check that GDB helpers work as expected
================================================

This directory hosts big tests that checks all aspects of GDB helpers:
Langkit-defined commands, pretty-printers, etc. To achieve this in a simple way
(i.e. not creating a big realistic language spec) we just create dummy
properties that implement the control-flow we want, or that deal with the types
for which we need to check pretty-printers: our goal here is just to cover GDB
helpers features.

`main.adb` does nothing by default, but exercise specific Lkt features when run
with subcommands. Post scripts (`check_*.py`) run that main under GDB to check
the helpers.  We keep this part in separate scripts to make it convenient, for
debugging purposes, to run these checks without re-building the
library/program.

Because they rely heavily on patten checking on the GDB output, post scripts
output nothing when checks run as expected, and output error messages when the
checks fail.
