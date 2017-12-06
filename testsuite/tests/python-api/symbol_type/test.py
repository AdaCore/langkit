"""
Test that Symbol bindings in the Python API are properly working.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, SymbolType, TokenType
from langkit.expressions import langkit_property
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field(type=TokenType)

    @langkit_property(public=True, return_type=SymbolType)
    def sym(sym=SymbolType):
        return sym


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Identifier, keep=True)),
)

build_and_run(foo_grammar, 'main.py')
print('Done')
