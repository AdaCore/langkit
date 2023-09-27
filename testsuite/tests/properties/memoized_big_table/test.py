"""
Check that big memoization tables are properly free'd. This used to yield a
stack overflow.
"""

from langkit.dsl import ASTNode, Int
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=Int)
    def compute(i=Int):
        return i + 1


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
