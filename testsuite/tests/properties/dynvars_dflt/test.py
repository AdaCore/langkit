"""
Test that assigning a default value to a dynamic variable:

* allows bind-less calls in the property DSL;
* generates the expected public API.
"""

from langkit.dsl import ASTNode, Bool
from langkit.expressions import (
    AbstractKind,
    DynamicVariable,
    Self,
    langkit_property,
)

from utils import build_and_run


BoolVar = DynamicVariable('bool_var', Bool)


class RootNode(ASTNode):

    @langkit_property(
        public=True,
        return_type=Bool,
        dynamic_vars=[(BoolVar, True)],
        kind=AbstractKind.abstract,
    )
    def prop():
        pass


class ExampleNode(RootNode):

    # Check that inheritance works as expected
    @langkit_property(dynamic_vars=[(BoolVar, True)])
    def prop():
        return BoolVar

    @langkit_property(public=True)
    def prop2():
        return Self.prop


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
