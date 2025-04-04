from __future__ import annotations

import os.path
import re

from e3.env import Env

from gdb_session import GDBSession


env = Env()
gdb: GDBSession = None


def start_gdb(mode_arg: str) -> GDBSession:
    """
    Start the test program under GDB and prepare the session for testing.

    :param mode_arg: "Mode" argument to pass to the main program.
    """
    global gdb
    gdb = GDBSession(os.path.join("obj", f"main{env.build.os.exeext}"))
    gdb.test(f"start {mode_arg}", None)

    # Load the GDB helpers
    gdbinit_script = os.path.join("build", "gdbinit.py")
    gdb.test(f"source {gdbinit_script}", "")

    # Disable colors: they are too cumbersome to test and unlikely to break
    gdb.test("pi langkit.utils.colors.Colors.disable_colors()", "")

    return gdb


break_label = re.compile("# BREAK:([a-z_]+)$")

lkt_break_map: dict[str, int] = {}
"""
Mapping from breakpoint labels in "test.lkt" to the corresponding line numbers.
"""

thread_notif_pattern = r"@/(\[New Thread .*\])?/ @/(Thread \d+ hit )?/"
"""
"quotemeta" pattern for thread-related messages from GDB after a control-flow
command has returned.
"""


# Fill ``lkt_break_map``
with open("test.lkt") as f:
    for i, line in enumerate(f, 1):
        m = break_label.search(line)
        if m:
            lkt_break_map[m.group(1)] = i


def break_lkt(label: str) -> None:
    """
    Create a breakpoint in Lkt sources on the line that contains the break
    comment marker matching ``label``.
    """
    run_foobreak(f"test.lkt:{lkt_break_map[label]}")


def check_var_state(var_name: str, expected: str) -> None:
    """
    Print the given variable using the "foostate" command and check its output.
    """
    gdb.test(f"foostate/f {var_name}", f"{var_name} = {expected}")


def run_continue() -> None:
    """
    Run the "continue" command.
    """
    gdb.test("continue", "Continuing.@...Breakpoint @NUMBER,@...")


def run_foobreak(spec: str) -> None:
    """
    Create a breakpoint using the "foobreak" command.
    """
    gdb.test(f"foobreak {spec}", "Breakpoint @NUMBER at @HEX@...")


def run_foonext(next_descr: str | None) -> None:
    """
    Run the "foonext" command, checking that the message describing the
    transition matches ``next_descr`` (unless left to None).
    """
    if next_descr is None:
        next_descr = "@..."
    gdb.test("foonext", f"{thread_notif_pattern}Breakpoint @...\n{next_descr}")


def run_fooout(next_descr: str) -> None:
    """
    Run the "fooout" command, checking that there was a control flow change and
    that the message describing the transition matches ``next_descr``.
    """
    gdb.test("fooout", f"{thread_notif_pattern}Breakpoint @...\n{next_descr}")


def run_foosi(next_descr: str) -> None:
    """
    Run the "foosi" command, checking that the message describing the
    transition matches ``next_descr``.
    """
    gdb.test("foosi", f"{thread_notif_pattern}Breakpoint @...\n{next_descr}")
