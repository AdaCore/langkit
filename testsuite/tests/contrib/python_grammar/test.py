"""
RA22-015: check the Python grammar's concrete syntax.
"""

from __future__ import absolute_import, division, print_function

import os
import sys

from utils import emit_and_print_errors, langkit_root


# Make the Python grammar importable
sys.path.append(os.path.join(langkit_root, 'contrib', 'python'))


from language import lexer, parser


emit_and_print_errors(parser.python_grammar, lexer.python_lexer,
                      unparse_cs=True)
