"""
Check that the railroad diagrams pass at least works without crashing.
"""

import os
import sys

from utils import emit_and_print_errors, langkit_root


# Make the Python grammar importable
sys.path.append(os.path.join(langkit_root, 'contrib', 'python'))

from language import lexer, parser


# TODO: We don't yet have a way to have a GH actions only test, so we just
#  ignore if the library is not found.
try:
    import railroad
    del railroad
    emit_and_print_errors(
        parser.python_grammar, lexer.python_lexer,
        explicit_passes_triggers={'emit railroad diagrams': True}
    )
except ImportError:
    pass
