"""
Test the generation of unparsers in a configuration where there is no single
cannonical parser that contains all information for unparsing. As a
consequence, this also checks that unparsers are properly combined.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class RootNode(FooNode):
    ident = Field()
    number = Field()


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    generate_unparser=True,
    types_from_lkt=True,
)

print("Done")
