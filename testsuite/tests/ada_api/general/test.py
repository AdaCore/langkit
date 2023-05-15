"""
General testing for the public Ada API.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class DeclError(FooNode):
    enum_node = True
    qualifier = True


class Identifier(FooNode):
    token_node = True


class Decl(FooNode):
    id = Field(type=Identifier)
    error = Field(type=DeclError)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
