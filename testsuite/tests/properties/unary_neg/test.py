"""
Test the handling of the unary "-" operator.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import BigIntLiteral, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True)
    def negate_int(i=T.Int):
        return -i

    @langkit_property(public=True)
    def minus_one_int():
        return -1

    @langkit_property(public=True)
    def negate_bigint(i=T.BigInt):
        return -i

    @langkit_property(public=True)
    def minus_one_bigint():
        return BigIntLiteral(-1)


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
