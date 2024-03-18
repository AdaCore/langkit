"""
Check that the handling of Unicode for various parsing settings (get from
file/buffer, encoding, file reader, ...) works correctly.
"""

from langkit.dsl import ASTNode, Field, T

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    f = Field(type=T.StrLit)


class StrLit(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)

print("Done")
