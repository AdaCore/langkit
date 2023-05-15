"""
Test the generation of unparsers involving Or parsers that contain Skip parsers
as alternatives.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class DefNode(FooNode):
    name = Field()
    value = Field()


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


class ErrorDef(FooNode):
    error_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    generate_unparser=True,
    types_from_lkt=True,
)
print("Done")
