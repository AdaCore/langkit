"""
Test that top-level Row parsers are properly reported as errors.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    tok = Field()


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Row(List(Tok(Token.Number, keep=True) ^ Literal)),
)
emit_and_print_errors(grammar)
print('Done')
