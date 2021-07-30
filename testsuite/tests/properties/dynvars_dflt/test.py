"""
Test that assigning a default value to a dynamic variable:

* allows bind-less calls in the property DSL;
* generates the expected public API.
"""

from langkit.dsl import ASTNode, Bool
from langkit.expressions import DynamicVariable, Self, langkit_property

from utils import build_and_run


BoolVar = DynamicVariable('bool_var', Bool)


class RootNode(ASTNode):
    pass


class ExampleNode(RootNode):
    @langkit_property(public=True, dynamic_vars=[(BoolVar, True)])
    def prop():
        return BoolVar

    @langkit_property(public=True)
    def prop2():
        return Self.prop


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
