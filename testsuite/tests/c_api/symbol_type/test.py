"""
Test that Symbol bindings in the C API are working properly.
"""

from langkit.dsl import ASTNode, Symbol
from langkit.expressions import langkit_property
from langkit.expressions import Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=Symbol)
    def sym():
        return Self.symbol

    @langkit_property(public=True, return_type=Symbol.array)
    def sym_array():
        return ["a", "b", "c"]


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.c"],
    types_from_lkt=True,
)
print("Done")
