"""
Test that the various wrappers that the Python binding instantiates (contexts,
units, nodes) are re-used whenever we want to wrap unique C values.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    examples = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
