#! /usr/bin/env python

"""
Utility script to run a command, and propagate potential GNU style errors as GH
errors.
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


def wrap_line(line):
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

while True:
    reads = [p.stdout.fileno(), p.stderr.fileno()]
    ret = select.select(reads, [], [])

    for fd in ret[0]:
        if fd == p.stdout.fileno():
            read = p.stdout.read()
            sys.stdout.write(read)
            stdout.append(read)
        if fd == p.stderr.fileno():
            read = p.stderr.read()
            sys.stderr.write(read)
            stderr.append(read)

    if p.poll() is not None:
        break

for line in "".join(stdout + stderr).splitlines():
    wrap_line(line)

sys.exit(exit_status)
