"""
Check that the scope of a Qualifier expression is correctly finalized on early
exit.
"""

from langkit.dsl import ASTNode
from langkit.expressions import BigIntLiteral, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def get_bigint():
        return BigIntLiteral(1)

    @langkit_property(public=True)
    def check():
        return Self.children.all(
            # This condition is always False, so we have a loop early exit,
            # which used to leave the result of "c.get_bigint" (big integers
            # are ref-counted) allocated when exitting the scope: the finalizer
            # for that scope was not called in that case.
            lambda c: c.get_bigint() == BigIntLiteral(0)
        )


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=False,
)
print("Done")
