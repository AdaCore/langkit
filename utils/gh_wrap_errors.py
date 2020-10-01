#! /usr/bin/env python

"""
Utility script to run a command, and propagate potential GNU style errors as
GitHub workflow commands.
"""

import argparse
from os import path as P
import re
import select
import subprocess
import sys


parser = argparse.ArgumentParser(description=__doc__)
_, extra_args = parser.parse_known_args()

path = P.dirname(P.realpath(__file__))
msg_re = re.compile(r"(.*?):(\d+):(?:(d+)\:)?(.*)?$")

p = subprocess.Popen(
    extra_args, universal_newlines=True,
    stdout=subprocess.PIPE, stderr=subprocess.PIPE
)

exit_status = 0


def emit_workflow_command_for_line(line):
    """
    If line is a GNU style error message, emit a corresponding workflow
    command.
    """
    global exit_status
    m = msg_re.match(line)
    if m:
        exit_status = 1
        file, line, _, msg = m.groups()
        if file.startswith('/'):
            file = P.relpath(file)
        print(f"::error file={file},line={line}::{msg}")


stdout = []
stderr = []

# Record the subprocess's stdout & stderr while still feeding them to this
# process' out & err.
while True:
    descs, _, _ = select.select((p.stdout.fileno(), p.stderr.fileno()), [], [])

    # Record stdout and stderr, while still forwarding them to this process'
    # stdout and stderr.
    for fd in descs:
        if fd == p.stdout.fileno():
            read = p.stdout.read()
            sys.stdout.write(read)
            stdout.append(read)
        elif fd == p.stderr.fileno():
            read = p.stderr.read()
            sys.stderr.write(read)
            stderr.append(read)

    returncode = p.poll()
    if returncode is not None:
        # Process has terminated, check if the return code is non zero, and if
        # it is, propagate to exit_status.
        if returncode != 0:
            exit_status = returncode
        break

for line in "".join(stdout + stderr).splitlines():
    emit_workflow_command_for_line(line)

sys.exit(exit_status)
