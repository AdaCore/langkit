"""
Check that warnings are correctly issued when a dynamic variable is bound but
is not used in the expression.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, LexicalEnv, LogicVar, T, UserField
from langkit.expressions import (Bind, DynamicVariable, Property, Self, Var,
                                 langkit_property, ignore)
from langkit.parsers import Grammar, Or

from utils import emit_and_print_errors

dyn_node = DynamicVariable('dyn_node', T.FooNode)
dyn_node_2 = DynamicVariable('dyn_node_2', T.FooNode)


class FooNode(ASTNode):
    @langkit_property(public=True)
    def test_prop_1():
        return dyn_node.bind(Self, Self.helper_1)

    @langkit_property(public=True)
    def helper_1():
        return True

    @langkit_property(public=True, dynamic_vars=[dyn_node_2])
    def test_prop_2():
        return dyn_node.bind(Self, Self.helper_2)

    @langkit_property(public=True, dynamic_vars=[dyn_node_2])
    def helper_2():
        return True

    @langkit_property(public=True)
    def test_prop_3():
        return dyn_node.bind(Self, dyn_node)

    @langkit_property(public=True)
    def test_prop_4():
        return dyn_node_2.bind(Self, Self.helper_2)


class Example(FooNode):
    pass


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example')
)
emit_and_print_errors(grammar)

print('Done')
