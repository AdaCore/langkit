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
import sys
from utils import build_and_run


LK_PYTHON_LIB_DIR = P.join(
    os.environ['LANGKIT_ROOT_DIR'], 'contrib', 'python'
)
sys.path.append(LK_PYTHON_LIB_DIR)

from language import lexer, parser


# RA22-015: This will also dump the python grammar, which is not dumped as part
# of another test, so keep it.
build_and_run(parser.python_grammar, "main.py", lexer=lexer.python_lexer,)
