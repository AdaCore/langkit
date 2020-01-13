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

o = subprocess.check_output(
    [sys.executable, P.join(LK_LIB_DIR, 'manage.py'), 'make', '-P'],
    # CWD is the lib's directory so that lib is generated in the lib's dir, not
    # in the test dir.
    cwd=LK_LIB_DIR
)

# TODO: stub test, eventually should try and parse all the
# expected_concrete_syntax.lkt files from the testsuite.
test_py = """
"Grammar for our language"
grammar None_grammar {
    "Root decl"
    decl <- Decl(
        ?@Plus Name(@Identifier) @LPar ref_list @RPar
    )
    main_rule <- list*(decl)
    ref <- Ref(Name(@Identifier))
    ref_list <- list+(ref)

}
"""

subprocess.check_call(
    [P.join(LK_LIB_DIR, 'build', 'bin', 'parse'), test_py]
)
