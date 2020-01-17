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

o = subprocess.check_output(
    [sys.executable, P.join(LK_LIB_DIR, 'manage.py'), 'make', '-P'],
    # CWD is the lib's directory so that lib is generated in the lib's dir, not
    # in the test dir.
    cwd=LK_LIB_DIR
)

tests = sorted(((P.join(root, f), P.basename(root))
                for root, _, files in os.walk(TESTS_DIR)
                for f in files
                if f == 'expected_concrete_syntax.lkt'), key=lambda x: x[1])

# TODO: for the moment we'll use a whitelist for tests, eventually we want to
# parse them all.
test_whitelist = [
    'dflt_arg_val', 'rewriting', 'ghost_nodes', 'import_argcount',
    'symbol_type', 'unicode_buffer', 'array_types', 'entity_eq',
    'custom_parsing_rule', 'node_negative_index', 'node_none_check',
    'tokens', 'unit_filename', 'wrapper_caches', 'unit_canon',
]
whitelisted_tests = [t for t in tests if t[1] in test_whitelist]

for full_syntax_path, test_name in whitelisted_tests:
    header = 'Parsing concrete syntax for test {}'.format(test_name)
    print("{}\n{}\n".format(header, "=" * len(header)))
    sys.stdout.flush()
    subprocess.check_call(
        [P.join(LK_LIB_DIR, 'build', 'bin', 'parse'), "-f",
         full_syntax_path]
    )
    print()
