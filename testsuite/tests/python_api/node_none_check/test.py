"""
Test that the bool() operator on nodes works as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
