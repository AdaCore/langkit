"""
Check that the Iterable aspect on the Ada API's Children_And_Trivia type
correctly supports empty list of children.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


class Name(FooNode):
    id = Field(type=Identifier)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
