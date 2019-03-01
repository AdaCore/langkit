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
    def prop1():
        return dyn_node.bind(Self, Self.prop2)

    @langkit_property(public=True)
    def prop2():
        return True

    @langkit_property(public=True, dynamic_vars=[dyn_node_2])
    def prop3():
        return dyn_node.bind(Self, Self.prop4)

    @langkit_property(public=True, dynamic_vars=[dyn_node_2])
    def prop4():
        return True

    @langkit_property(public=True)
    def prop5():
        return dyn_node.bind(Self, dyn_node)

    @langkit_property(public=True)
    def prop6():
        return dyn_node_2.bind(Self, Self.prop4)

class Example(FooNode):
    pass

grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example')
)
emit_and_print_errors(grammar)

print('Done')
