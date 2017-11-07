"""
Test that garbage tokens left after the main parsing rule completes does not
crash. It used to!
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    tok = Field()


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Literal(Tok(Token.Number, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
