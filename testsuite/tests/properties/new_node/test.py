"""
Test AST node synthetization and a basic use of it in the Python API.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, TokenType, synthetic
from langkit.expressions import New, Self, langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    tok = Field()


@synthetic
class SynthNode(FooNode):
    name = Field(type=TokenType)
    items = Field(type=Literal.list)


class LiteralSequence(FooNode):
    name = Field()
    items = Field()

    @langkit_property(memoized=True)
    def new_node():
        return New(SynthNode, name=Self.name, items=Self.items)

    @langkit_property(public=True)
    def prop():
        return Self.new_node.as_bare_entity


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=foo_grammar.list_rule,
    list_rule=LiteralSequence(
        '(',
        Tok(Token.Identifier, keep=True),
        List(foo_grammar.list_item, sep=','),
        ')'
    ),
    list_item=Literal(Tok(Token.Number, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
