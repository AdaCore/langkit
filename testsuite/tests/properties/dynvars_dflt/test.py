"""
Test that assigning a default value to a dynamic variable generates the
expected public API.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, BoolType
from langkit.expressions import DynamicVariable, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


BoolVar = DynamicVariable('bool_var', BoolType)


class RootNode(ASTNode):
    pass


class ExampleNode(RootNode):
    @langkit_property(public=True, dynamic_vars=[(BoolVar, True)])
    def prop():
        return BoolVar

g = Grammar('main_rule')
g.add_rules(main_rule=ExampleNode('example'))
build_and_run(g, py_script='main.py')

print('Done')
