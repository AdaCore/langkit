"""
Test that true alternatives for booleanized Opt parsers do not yield ghost
nodes.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class HasError(FooNode):
    enum_node = True
    qualifier = True


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              ada_main='main.adb', generate_unparser=True)
print('Done')
