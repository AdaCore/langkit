"""
Check that memoized properties that have arguments of array types work as
advertised.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import ArrayLiteral, If, String, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=T.Int.array)
    def get_array():
        return ArrayLiteral([1, 2])

    @langkit_property(public=True, memoized=True, return_type=T.Int)
    def test_prop(numbers=T.Int.array, c=T.String):
        return If(c == String("one"), numbers.at(0), numbers.at(1))

    @langkit_property(public=True, memoized=True, return_type=T.Bool)
    def test_prop2(numbers=T.FooNode.entity.array):
        return numbers.length == 0


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
