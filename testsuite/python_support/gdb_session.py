from __future__ import annotations

import atexit
import inspect
import os
import re
import traceback
from typing import Optional

from expect import EXPECT_DIED, EXPECT_TIMEOUT, ExpectProcess
from quotemeta import convert_expression
from utils import indent


class GDBSession:
    """
    Handle for a GDB session, to run GDB commands and inspect their output.
    """

    PROMPT_RE = r"\(gdb\) "
    TIMEOUT = 30  # In seconds

    def __init__(self,
                 program: Optional[str] = None,
                 log_file: Optional[str] = None):

        # Make sure that the GDB subprogram is terminated with its logs written
        # somewhere before the end of the script.
        atexit.register(self.stop)

        self.log_file = log_file or "gdb.log"

        # Disable the load of .gdbinit to avoid user configuration
        # interference.
        argv = ["gdb", "--nh"]

        os.environ["TERM"] = "dumb"

        self.proc = ExpectProcess(argv, save_input=True, save_output=True)
        self.alive = True
        _ = self._read_to_next_prompt()

        # Enable Python backtraces to ease investigation
        self.execute("set python print-stack full")

        # Disable interactive mode, which is bound to create trouble in a
        # testsuite.
        self.execute("set interactive-mode off")

        # Make the output deterministic, independent of the actual terminal
        # size.
        self.execute("set height 0")
        self.execute("set width 80")

        if program:
            # Only then, load the inferior. Loading gnatdbg before checks that
            # importing it does not rely on the presence of debug information.
            self.test("file {}".format(program),
                      r"Reading symbols from {}...@...@/done|/"
                      .format(program))

    def _read_to_next_prompt(self) -> str:
        """
        Read GDB's output until we reach the next prompt.

        Return the output in between. Raise a RuntimeError if GDB dies or if
        timeout is reached.
        """
        assert self.alive
        status = self.proc.expect([self.PROMPT_RE], self.TIMEOUT)
        if status is EXPECT_DIED:
            raise RuntimeError("GDB died")
        elif status is EXPECT_TIMEOUT:
            raise RuntimeError("Timeout reached while waiting for GDB")

        assert status == 0
        out, prompt = self.proc.out()
        return out

    def execute(self, command: str) -> None:
        """
        Shortcut for `test` without an expected output.
        """
        return self.test(command, None)

    def test(self, command: str, expected_output: Optional[str]) -> None:
        """
        Send the given command to GDB and check its output.

        :param command: GDB command to send.
        :param expected_output: If None, don't check the command output.
            Otherwise, it must be a quotemeta expression that must match the
            output.
        """
        assert self.alive
        assert self.proc.send(command)
        output = self._read_to_next_prompt().strip().replace("\r", "")
        matcher = (
            ""
            if expected_output is None else
            convert_expression(expected_output)
        )
        if (
            expected_output is not None
            and not re.match(matcher, output)
        ):
            print("")
            print("FAIL: {}".format(command))
            print("From:")
            # Print the current call stack but omit the current frame
            print("".join(
                traceback.format_stack(inspect.currentframe().f_back)
            ).rstrip())
            print("Output:")
            print(indent(output))
            print("Does not match the expected:")
            print(indent(expected_output))

    def print_expr(self, expr: str, expected_output: str) -> None:
        """
        Execute the "print" GDB command and check its output.

        :param expr: Expression to print.
        :param expected_output: Regular expression that the output must
            match. Note that it must not include the '$NUMBER = ' prefix.
        """
        self.test("print {}".format(expr), "$@NUMBER = " + expected_output)

    @staticmethod
    def find_loc(filename: str, slug: str) -> str:
        """
        Look for a source file location.

        If the location is found, return a string location suitable for GDB's
        break command. Raise a RuntimeError otherwise.

        :param filename: Target source file name.
        :param slug: Source file excerpt for the location. For instance, a
            specific string that is in a comment.
        :return: String location suitable for GDB's break command.
        """
        with open(filename, "r") as f:
            for i, line in enumerate(f, 1):
                if slug in line:
                    return "{}:{}".format(filename, i)
        raise RuntimeError(
            "Could not find location in {} for {}".format(filename, slug)
        )

    def run_to(self, location: str) -> None:
        """
        Start inferior execution until it reaches the given location.

        :param location: String location suitable for GDB's break command.
        """
        self.execute("tbreak {}".format(location))
        self.execute("run")

    def kill(self) -> None:
        """
        Kill the inferior process currently running.
        """
        self.test("kill",
                  "Kill the program being debugged? (y or n)"
                  " [answered Y; input not from terminal]\n"
                  "[Inferior 1 (process @/\\d+/) killed]")

    def stop(self) -> None:
        """
        Stop GDB.

        This writes session logs to make post-mortem debugging.
        """
        if not self.alive:
            return

        self.alive = False

        # No matter what, write the session logs to make post-mortem debugging
        # possible.
        with open(self.log_file, "w") as f:
            f.write(self.proc.get_session_logs())
