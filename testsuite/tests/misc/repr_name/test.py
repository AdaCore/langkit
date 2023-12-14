"""
Check that the "repr_name" node annotation works as expected.
"""

from langkit.dsl import ASTNode, Annotations, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True
    annotations = Annotations(repr_name="Id")


class Decl(FooNode):
    name = Field(type=Name)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
