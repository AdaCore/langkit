"""
Test that big integers work as expected in the DSL.
"""

from langkit.dsl import ASTNode, BigInt, Field, T, abstract
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import (
    AbstractProperty, BigIntLiteral, ExternalProperty, If, Self,
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


class Decl(FooNode):
    name = Field(type=T.Identifier)
    expr_tree = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self)
    )


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


class Ref(Expr):
    name = Field()

    @langkit_property()
    def evaluate():
        return (Self.node_env.get_first(Self.name.symbol)
                .cast(T.Decl).expr_tree.evaluate)


class Plus(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)

    @langkit_property()
    def evaluate():
        return Self.left.evaluate + Self.right.evaluate


class Minus(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)

    @langkit_property()
    def evaluate():
        return Self.left.evaluate - Self.right.evaluate


class Equal(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)

    @langkit_property()
    def evaluate():
        return If(Self.left.evaluate == Self.right.evaluate,
                  BigIntLiteral(1),
                  BigIntLiteral(0))


class LessThan(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)

    @langkit_property()
    def evaluate():
        return BigIntLiteral(If(Self.left.evaluate < Self.right.evaluate,
                                1, 0))


# The real test is the Python script, but we use the Ada program to check for
# memory leaks in properties implementation.
build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
)
print("Done")
