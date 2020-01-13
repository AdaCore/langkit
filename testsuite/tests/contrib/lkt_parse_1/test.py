"""
This tests that the Langkit based python parser can be compiled and can parse a
simple python program without errors.

TODO: This is hackish and minimalistic. Since Langkit's python parser is a
fully fledged Langkit generated lib, it would make sense to reuse Libadalang's
parser test driver.
"""

from __future__ import absolute_import, division, print_function

import os
from os import path as P
import subprocess
import sys


LK_LIB_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'contrib', 'lkt')
TESTS_DIR = P.join(os.environ['LANGKIT_ROOT_DIR'], 'testsuite', 'tests')

o = subprocess.check_output(
    [sys.executable, P.join(LK_LIB_DIR, 'manage.py'), 'make', '-P'],
    # CWD is the lib's directory so that lib is generated in the lib's dir, not
    # in the test dir.
    cwd=LK_LIB_DIR
)

# TODO: for the moment we'll use a whitelist for tests, eventually we want to
# parse them all.
test_whitelist = ['dflt_arg_val']

for root, _, files in os.walk(TESTS_DIR):
    test_name = P.basename(root)
    for f in files:
        if (f == 'expected_concrete_syntax.lkt'
                and test_name in test_whitelist):
            subprocess.check_call(
                [P.join(LK_LIB_DIR, 'build', 'bin', 'parse'), "-f",
                 P.join(root, f)]
            )
