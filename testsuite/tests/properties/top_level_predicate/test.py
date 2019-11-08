"""
Test that we create Predicate on properties of the root AST node.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, LogicVar, UserField
from langkit.expressions import Predicate, Self, Var, ignore, langkit_property
from langkit.parsers import Grammar

from utils import build


class FooNode(ASTNode):
    @langkit_property()
    def sophisticated_predicate():
        return True


class Example(FooNode):
    var1 = UserField(LogicVar, public=False)

    @langkit_property(public=True)
    def prop():
        ignore(Var(Predicate(FooNode.sophisticated_predicate, Self.var1)))
        return Self.as_bare_entity


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example'),
)
build(grammar)

print('Compilation was successful')
