"""
Check that the ".any_of" expression works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property()
    def identity(i=T.Int):
        return i

    @langkit_property(public=True, return_type=T.Bool)
    def in_1(i=T.Int):
        return Self.identity(i).any_of(1)

    @langkit_property(public=True, return_type=T.Bool)
    def in_1_3_4(i=T.Int):
        return Self.identity(i).any_of(1, 3, 4)

    @langkit_property(public=True, return_type=T.Bool)
    def in_nodes(n1=T.FooNode, n2=T.FooNode):
        return Self.any_of(n1, n2)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=False,
)
print("Done")
