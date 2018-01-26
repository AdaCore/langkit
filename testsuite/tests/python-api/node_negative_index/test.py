"""
Test the handling of negative indexes in the Python binding of AST nodes child
getters.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    tok = Field(type=T.TokenType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.name),
    name=Name(Token.Identifier),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
