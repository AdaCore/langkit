"""
Check the Closest_Common_Parent node primitive.
"""

from langkit.dsl import ASTNode, Field, T

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Cons(FooNode):
    left = Field(type=T.FooNode)
    right = Field(type=T.FooNode)


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=False,
)
print("Done")
