"""
Test the handling of the "or?" binary operator.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.FooNode.entity)
    def compute(n1=T.FooNode.entity, n2=T.FooNode.entity, n3=T.FooNode.entity):
        return n1._or(n2._or(n3))


class Identifier(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
