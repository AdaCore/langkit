"""
Test that garbage tokens left after the main parsing rule completes does not
crash. It used to!
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
