############################################################################
#                                                                          #
#                            EXPECT.PY                                     #
#                                                                          #
#              Copyright (C) 2010-2014 Ada Core Technologies, Inc.         #
#                                                                          #
# This program is free software: you can redistribute it and/or modify     #
# it under the terms of the GNU General Public License as published by     #
# the Free Software Foundation, either version 3 of the License, or        #
# (at your option) any later version.                                      #
#                                                                          #
# This program is distributed in the hope that it will be useful,          #
# but WITHOUT ANY WARRANTY; without even the implied warranty of           #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            #
# GNU General Public License for more details.                             #
#                                                                          #
# You should have received a copy of the GNU General Public License        #
# along with this program.  If not, see <http://www.gnu.org/licenses/>     #
#                                                                          #
############################################################################

"""Expect like features.

The module provides an API similar to TCL/Expect.
"""

from __future__ import annotations

import datetime
import logging
import pexpect
import re
import sys
from time import sleep
from typing import List, Match, Optional, Protocol, Tuple, Union

from e3.os.fs import which

EXPECT_TIMEOUT = -2
EXPECT_DIED = -3

logger = logging.getLogger("gnatpython.expect")


class ExpectError(Exception):
    """Expect exception."""

    def __init__(self, cmd: str, msg: str):
        super().__init__(self, cmd, msg)
        self.cmd = cmd
        self.msg = msg

    def __str__(self) -> str:
        return "%s: %s" % (self.cmd, self.msg)


class TermEOF(Exception):
    """A local subsitute for the pexpect.EOF exception."""

    def __init__(self) -> None:
        super().__init__(self, "")


class TermTIMEOUT(Exception):
    """A local subsitute for the pexpect.TIMEOUT exception."""

    def __init__(self) -> None:
        super().__init__(self, "")


TIMEOUT = pexpect.TIMEOUT
EOF = pexpect.EOF

Timeout = Union[int, float]


class PexpectProcess:
    """Stub for pexpect's process objects."""

    pid: int

    def expect(self,
               pattern: Union[str, List[str]],
               timeout: Optional[int]) -> None: ...
    def send(self, s: str) -> int: ...
    def read_nonblocking(self,
                         size: int = 1,
                         timeout: Timeout = -1) -> str: ...
    def terminate(self, force: bool = False) -> bool: ...
    def wait(self) -> int: ...
    def sendintr(self) -> None: ...


class SpawnFunction(Protocol):
    def __call__(self,
                  command: str,
                  arguments: List[str],
                  timeout: Optional[int]) -> PexpectProcess: ...


spawn: SpawnFunction

if sys.platform == "win32":
    # On Windows we need to have a small wrapper around the
    # PopenSpawn class.
    from pexpect.popen_spawn import PopenSpawn
    import signal

    class WindowsSpawn(PopenSpawn):
        def __init__(self, command: str, arguments: List[str], timeout: int):
            PopenSpawn.__init__(
                self, [command] + arguments, encoding="cp1252", timeout=timeout
            )

        def terminate(self, force: bool) -> None:
            # Ignore the parameter.
            # The kill method doesn't work correctly on Windows,
            # so we need to use terminate here.
            self.proc.terminate()

        def sendintr(self) -> None:
            # The kill method doesn't work correctly on Windows,
            # so we need to use send_signal here.
            self.proc.send_signal(signal.CTRL_C_EVENT)

    spawn = WindowsSpawn

else:
    # On Unix hosts we can use the ordinary pexpect.
    def unix_spawn(command: str,
                   arguments: List[str],
                   timeout: Optional[int]) -> PexpectProcess:
        result = pexpect.spawn(
            command, arguments, timeout=timeout, encoding="UTF-8"
        )
        result.setecho(False)
        return result

    spawn = unix_spawn


class ExpectProcess(object):
    """Expect Main class.

    ATTRIBUTES
      command_line: list of strings containg the command line used to spawn the
        process.
      status: The return code. None while the command is still running, and an
        integer after method "close" has been called.
    """

    def __init__(self,
                 command_line: List[str],
                 save_output: bool = False,
                 save_input: bool = False):
        """Constructor.

        :param command_line: list of strings representing the command line to
            be spawned.
        :type command_line: list[str]
        :param save_output: Save all output generated during the session for
            later retrieval (see method get_session_logs).
        :type save_output: bool
        :param save_input: Save all input generated during the session for
            later retrieval (see method get_session_logs).
        :type save_input: bool
        """
        self.save_output = save_output
        self.save_input = save_input

        # Convert the command line to a list of string is needed
        command_line = [str(arg) for arg in command_line]
        if len(command_line) < 1:
            raise ExpectError("__init__", "expect a non empty list as argument")

        command_line[0] = which(command_line[0])

        # Store the command line used
        logger.debug("spawn %s" % " ".join(command_line))
        self.command_line = command_line

        # Spawn the process.
        self._child: Optional[PexpectProcess] = spawn(command_line[0], command_line[1:], timeout=None)
        self.pid = self._child.pid

        # Initialize our buffer
        self.buffer = ""

        # If we have to save input or output keep another buffer that
        # is never flushed.
        self.saved_buffer = ""

        # Keep the state of the process
        self.process_is_dead = False

        # This is where we store that last successful expect result
        self.last_match: Optional[Tuple[int, str, Match[str]]] = None

        # This is where the command returned status will be stored
        # when the command has exited.  For the moment, it is not
        # available.
        self.status: Optional[int] = None

    def __poll(self, timeout: Timeout) -> None:
        """Poll for new output.

        :param timeout: Maximum time in seconds we wait for new output.
        """
        assert self._child is not None
        try:
            result = self._child.read_nonblocking(size=16384, timeout=timeout)
            result = result.replace("\r", "")
            self.buffer += result
            if self.save_output:
                self.saved_buffer += result
        except TIMEOUT:
            pass
        except EOF:
            self.process_is_dead = True

    def flush(self) -> None:
        """Flush output buffer.

        Output read before the call to flush will be completely ignored
        in next expect calls.
        """
        self.__poll(0)
        self.buffer = ""

    def sendline(self, msg: str) -> int:
        """Send a line to the process.

        :param msg: String to be sent.

        :return: 1 of OK and 0 otherwise.
        """
        return self.send(msg + "\n")

    def send(self,
             msg: str,
             add_lf: bool = True,
             flush_buffer: bool = False) -> int:
        """Send a msg to the program.

        :param msg: the message to send
        :return: 1 if OK, 0 otherwise.
        :raise ExceptError: if the process has been closed
        """
        if self._child is None:
            raise ExpectError("send", "process has been closed")

        if add_lf:
            msg += "\n"

        if flush_buffer:
            self.flush()

        if self.save_input:
            self.saved_buffer += msg

        write_status = self._child.send(msg)
        if write_status <= 0:
            return 0
        else:
            return 1

    def expect(self, patterns: List[str], timeout: int) -> Optional[int]:
        """Expect an output.

        :param patterns: A list of regexp.
        :param timeout: Maximum time in seconds we wait for a match.

        :return: If a pattern was matched then it returns it position in
          the list of patterns. Otherwise the function returns EXPECT_DIED if
          the process die before matching one of the pattern, or
          EXPECT_TIMEOUT if none of the patterns were matched during timeout
          seconds.
        """
        if self._child is None:
            raise ExpectError("expect", "process has been closed")

        match = None
        result = 0
        expect_start = datetime.datetime.utcnow()
        time_left: Timeout = int(timeout * 1000.0)

        while match is None and time_left > 0:
            # Do we have a match with the current output
            for index, pattern in enumerate(patterns):
                match = re.search(pattern, self.buffer)
                if match is not None:
                    result = index
                    break

            if match is not None:
                break
            else:
                # We don't have a match so poll for new output
                self.__poll(time_left / 1000)
                if self.process_is_dead:
                    return EXPECT_DIED

                # update time_left.
                # The update is done only if current time is superior to time
                # at which the function started. This test might seem a bit
                # weird but on some Linux machines on VmWare we have found
                # huge clock drift that the system tries to compensate. The
                # consequence is that we cannot assume that the clock is
                # monotonic.
                current_time = datetime.datetime.utcnow()
                if current_time > expect_start:
                    time_spent = current_time - expect_start
                    time_left = int(timeout * 1000.0) - (
                        time_spent.seconds * 1000 + time_spent.microseconds / 1000
                    )

        if match is not None:
            self.last_match = (result, self.buffer[: match.start(0)], match)
            self.buffer = self.buffer[match.end(0) :]
            return result

        if time_left <= 0:
            return EXPECT_TIMEOUT

        return None

    def out(self) -> Tuple[str, str]:
        """Retrieve matched output.

        :return: a tuple of strings. The first element is the output read
            before the match and the second element the output matched by
            the regexp. If if there is no previous match then it return
            ("", "")
        """
        if self.last_match is None:
            return ("", "")
        else:
            return (self.last_match[1], self.last_match[2].group(0))

    def shutdown(self) -> None:
        """Ensure that if python ends all expect object are killed.

        This is specially important when the python process doing the expect
        is interrupted by a Ctrl-C. This method ensure that we don't let
        expect processes alive.
        """
        self.close()

    def close(self) -> None:
        """Close an expect session.

        If the underlying process is not dead yet, kill it. Set the
        status attribute to the command return code.
        """
        if self._child is not None:
            self.interrupt()
            sleep(0.05)
            if not self.process_is_dead:
                self._child.terminate(force=True)
            self.status = self._child.wait()
            self._child = None

    def interrupt(self) -> None:
        """Interrupt.

        This is an equivalent of sending Ctrl-C to the process
        """
        if not self.process_is_dead and self._child is not None:
            self._child.sendintr()

    def get_session_logs(self) -> str:
        """Return the saved output and/or input.

        :return: A string containing the logs of the session.

        The output and/or input are only available if the constructor was
        called with save_output and/or save_output respectively set to True
        (this is NOT the default).
        """
        return self.saved_buffer

    def set_timer(self, delay: Timeout) -> None:
        """Set timer.

        :param delay: A float or integer representing seconds.

        When timer is set, you can check its expiration with the
        has_timer_expired method.
        """
        self.timer_end = (
            datetime.datetime.utcnow() + datetime.timedelta(seconds=delay)
        )

    def has_timer_expired(self) -> bool:
        """Check if timer has expired.

        :return: True if timer has expired, False otherwise.
        """
        return self.timer_end < datetime.datetime.utcnow()

    def wait(self) -> int:
        """Wait for the end of the process and return the status.

        Note that the function is a direct wrapper around waitpid. While
        waiting for the process end, the output of the process will not be
        read so you should ensure before calling that function that not too
        much output will be generated (i.e less than 32Ko), otherwise the
        process might block.

        :return: The process exit status also stored in self.status.
        """
        if self._child is None:
            raise ExpectError("expect", "process has been closed")
        self.status = self._child.wait()
        self._child = None
        return self.status
