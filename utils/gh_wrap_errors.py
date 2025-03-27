#! /usr/bin/env python

"""
Utility script to run a command, and propagate potential GNU style errors as
GitHub workflow commands.
"""

import argparse
from os import path as P
import re
import subprocess
import sys


parser = argparse.ArgumentParser(description=__doc__)
_, extra_args = parser.parse_known_args()

path = P.dirname(P.realpath(__file__))
msg_re = re.compile(r"(.*?):(\d+):(?:(d+)\:)?(.*)?$")

p = subprocess.Popen(
    extra_args,
    universal_newlines=True,
    stdin=subprocess.DEVNULL,
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
)

# If a line is a GNU style error message, emit a corresponding workflow
# command and trigger an error status.
exit_status = 0
for line in p.stdout:
    m = msg_re.match(line.rstrip())
    if m:
        exit_status = 1
        file, line, _, msg = m.groups()
        if file.startswith("/"):
            file = P.relpath(file)
        print(f"::error file={file},line={line}::{msg}")
    sys.stdout.write(line)
    sys.stdout.flush()

p.wait()
sys.exit(p.returncode if exit_status == 0 else exit_status)
