"""
Test that nodes that contain empty lists with no token associated are properly
unparsed.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class DefNode(FooNode):
    name = Field()
    values = Field()


class Identifier(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    generate_unparser=True,
    types_from_lkt=True,
)
print("Done")
