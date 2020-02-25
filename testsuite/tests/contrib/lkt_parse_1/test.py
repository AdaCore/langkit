"""
Test langkit's new concrete syntax parser.

RA22-015: This is a temporary test, we'll get rid of it when tests are ported
to the new syntax.
"""

from __future__ import absolute_import, division, print_function

import os
from os import path as P
import subprocess
import sys


LK_LIB_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'contrib', 'lkt')
TESTS_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'testsuite', 'tests')

tests = sorted(((P.join(root, f), P.basename(root))
                for root, _, files in os.walk(TESTS_DIR)
                for f in files
                if f == 'expected_concrete_syntax.lkt'), key=lambda x: x[1])

for full_syntax_path, test_name in tests:
    header = 'Parsing concrete syntax for test {}'.format(test_name)
    print("{}\n{}\n".format(header, "=" * len(header)))
    sys.stdout.flush()
    subprocess.check_call(
        [P.join(LK_LIB_DIR, 'build', 'bin', 'parse'), "-f",
         full_syntax_path]
    )
    print()
