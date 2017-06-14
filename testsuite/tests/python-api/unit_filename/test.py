"""
Test getting the filename corresponding to an analysis unit.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, Token as TokenType
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field(type=TokenType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Example, keep=True)),
)

build_and_run(foo_grammar, 'main.py')

print('Done')
