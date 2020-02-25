"""
Test the behavior of the .is_a operation on null nodes.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def null_node():
        return No(T.FooNode)

    @langkit_property(public=True)
    def is_null_a_def():
        return Self.null_node.is_a(T.Def)

    @langkit_property(public=True)
    def is_null_a_def_or_example():
        return Self.null_node.is_a(T.Def, T.Example)


class Def(FooNode):
    example = Field()


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
