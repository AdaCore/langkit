"""
Regression test. We used to generate invalid Ada code when two lazy fields in
different nodes had the same name.
"""

from langkit.dsl import ASTNode
from langkit.expressions import lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True

    @lazy_field(public=True)
    def my_field():
        return 1


class Example(FooNode):
    token_node = True

    @lazy_field(public=True)
    def my_field():
        return 2


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
