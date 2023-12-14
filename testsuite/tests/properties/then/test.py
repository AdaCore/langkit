"""
Check that "then" expressions work as expected in properties.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def node_then(n=T.FooNode):
        return n.then(lambda nn: nn.parent)

    @langkit_property(public=True)
    def node_then_with_default(n=T.FooNode):
        return n.then(lambda nn: nn.parent, default_val=Self)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
