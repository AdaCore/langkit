"""
Test that Bind works when binding entities, and using an equality property that
takes entities.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, LogicVar, Int, UserField, T
from langkit.expressions import (
    AbstractProperty, Let, Property, Self, Bind, langkit_property
)
from langkit.parsers import Grammar

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    prop = AbstractProperty(runtime_check=True, type=Int, public=True)


class Literal(FooNode):
    token_node = True

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)
    var = UserField(LogicVar, public=False)

    @langkit_property(return_type=T.Literal.entity)
    def node():
        return Self.as_entity

    b = Property(Bind(Self.var, Self.a, Self.node))

    @langkit_property(public=True)
    def public_pro():
        return Let(lambda _=Self.b: Self.as_bare_entity)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Literal(Token.Number))
build_and_run(foo_grammar, 'main.py')
print('Done')
