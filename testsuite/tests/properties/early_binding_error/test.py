"""
Test that Adalog's Early_Binding_Error exception is turned into a
Property_Error exception.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, UserField, T
from langkit.expressions import Predicate, Self, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    a = UserField(T.LogicVarType, public=False)

    @langkit_property()
    def pred():
        return False

    @langkit_property(public=True)
    def do_solving():
        return Predicate(Example.pred, Self.a).solve


G = Grammar('main_rule')
G.add_rules(main_rule=Example('example'))
build_and_run(G, 'main.py')
print('Done')
