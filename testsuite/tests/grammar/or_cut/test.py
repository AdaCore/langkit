"""
Test that diagnostics emitted after a Cut are properly reset when the
Or parser which triggered the rule that created the diagonstics backtracks and
tries another alternative.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()


class BasicVar(FooNode):
    name = Field()


class Var(FooNode):
    basic_var = Field()
    value = Field()


class Name(FooNode):
    token_node = True


build_and_run(
    lkt_file="foo.lkt",
    gpr_mains=["main.adb"],
    unparse_script="to:foo.lkt,import:parser,nodes",
    types_from_lkt=True,
)
print("Done")
