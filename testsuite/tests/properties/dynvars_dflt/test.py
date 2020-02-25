"""
Test that assigning a default value to a dynamic variable generates the
expected public API.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool
from langkit.expressions import DynamicVariable, langkit_property

from utils import build_and_run


BoolVar = DynamicVariable('bool_var', Bool)


class RootNode(ASTNode):
    pass


class ExampleNode(RootNode):
    @langkit_property(public=True, dynamic_vars=[(BoolVar, True)])
    def prop():
        return BoolVar

build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
