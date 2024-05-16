"""
Test that diagnostics emitted after a Cut are properly reset when the
Or parser which triggered the rule that created the diagonstics backtracks and
tries another alternative.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run, unparse_script


class FooNode(ASTNode):
    pass


class VarDecl(FooNode):
    name = Field()
    value = Field()


class Name(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    unparse_script=unparse_script,
    types_from_lkt=True,
)
print("Done")
