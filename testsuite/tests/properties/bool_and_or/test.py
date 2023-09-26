"""
Check the hanling of "and" / "or" operators in properties.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    # In both "check_" properties, the second operand call (when n is null)
    # helps checking that the short-circuit case *prevents* the call from
    # happening.

    @langkit_property(public=True)
    def check_and(b1=T.Bool, b2=T.Bool, n=T.FooNode):
        return b1 & n.identity(b2)

    @langkit_property(public=True)
    def check_or(b1=T.Bool, b2=T.Bool, n=T.FooNode):
        return b1 | n.identity(b2)

    @langkit_property(public=True)
    def identity(b=T.Bool):
        return b


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
