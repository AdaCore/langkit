"""
Check that the ``find`` operation correctly handles collections of ref-counted
elements.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import BigIntLiteral, Self, langkit_property

from utils import build_and_run


# Define a struct that needs ref counting
class MyRecord(Struct):
    value = UserField(type=T.BigInt)


class FooNode(ASTNode):
    @langkit_property()
    def resolve_own():
        return MyRecord.new(value=BigIntLiteral(1))

    @langkit_property(public=True)
    def resolve():
        return Self.children.map(
            lambda c: c.resolve_own()
        ).find(
            lambda c: c.value == BigIntLiteral(1)
        )


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main="main.adb",
              types_from_lkt=False)
print('Done')
