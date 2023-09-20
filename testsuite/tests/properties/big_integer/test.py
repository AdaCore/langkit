"""
Test that big integers work as expected in the DSL.
"""

from langkit.dsl import ASTNode, BigInt, Field, T, abstract
from langkit.expressions import (
    AbstractProperty, BigIntLiteral, ExternalProperty, If, Self, Var,
    langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def identity(value=T.BigInt):
        return value

    @langkit_property(public=True)
    def check_big_literal():
        return BigIntLiteral(99999999999999999999999999999999999999999999)

    @langkit_property()
    def to_int(b=T.Bool):
        return If(b, BigIntLiteral(1), BigIntLiteral(0))


class Decl(FooNode):
    name = Field(type=T.Identifier)
    expr_tree = Field(type=T.Expr)


class Identifier(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    evaluate = AbstractProperty(type=BigInt, public=True)

    @langkit_property(public=True)
    def evaluate_as_int():
        return Self.evaluate.as_int


class Literal(Expr):
    token_node = True

    evaluate = ExternalProperty(uses_entity_info=False, uses_envs=False)


class ParenExpr(Expr):
    expr = Field(type=Expr)

    @langkit_property()
    def evaluate():
        return Self.expr.evaluate


class OpKind(FooNode):
    enum_node = True
    alternatives = [
        "plus",
        "minus",
        "equal",
        "less_than",
        "less_than_or_equal",
        "greater_than",
        "greater_than_or_equal",
    ]


class BinOp(Expr):
    left = Field(type=T.Expr)
    op = Field(type=OpKind)
    right = Field(type=T.Expr)

    @langkit_property()
    def evaluate():
        left = Var(Self.left.evaluate)
        right = Var(Self.right.evaluate)
        return Self.op.match(
            lambda _=OpKind.alt_plus: left + right,
            lambda _=OpKind.alt_minus: left - right,
            lambda _=OpKind.alt_equal: Self.to_int(left == right),
            lambda _=OpKind.alt_less_than: Self.to_int(left < right),
            lambda _=OpKind.alt_less_than_or_equal: Self.to_int(
                left <= right
            ),
            lambda _=OpKind.alt_greater_than: Self.to_int(left > right),
            lambda _=OpKind.alt_greater_than_or_equal: Self.to_int(
                left >= right
            ),
        )


# The real test is the Python script, but we use the Ada program to check for
# memory leaks in properties implementation.
build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
