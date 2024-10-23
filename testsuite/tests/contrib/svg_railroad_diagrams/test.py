"""
Check that the railroad diagrams pass at least works without crashing.
"""

import os
import sys

from utils import emit_and_print_errors, langkit_root


# Make the Python grammar importable
sys.path.append(os.path.join(langkit_root, 'contrib', 'python'))

from language import lexer, parser


emit_and_print_errors(
    parser.python_grammar,
    lexer.python_lexer,
    config={"optional_passes": {"emit railroad diagrams": True}},
)
