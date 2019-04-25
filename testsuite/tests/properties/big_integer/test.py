"""
Test that big integers work as expected in the DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, BigInt, Field, T, abstract
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import (
    AbstractProperty, BigIntLiteral, ExternalProperty, If, New, Self,
    langkit_property
)
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def identity(value=T.BigInt):
        return value


class Decl(FooNode):
    name = Field(type=T.Identifier)
    expr_tree = Field(type=T.Expr)

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self)
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
        return BigIntLiteral(If(Self.left.evaluate == Self.right.evaluate,
                                1, 0))


class LessThan(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)

    @langkit_property()
    def evaluate():
        return BigIntLiteral(If(Self.left.evaluate < Self.right.evaluate,
                                1, 0))


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.decl),
    decl=Decl('def', g.name, '=', g.expr),

    expr=Or(g.op, g.atom),

    op=Or(Plus(g.atom, '+', g.expr),
          Minus(g.atom, '-', g.expr),
          Equal(g.atom, '=', g.expr),
          LessThan(g.atom, '<', g.expr)),

    atom=Or(g.ref, g.literal),
    ref=Ref(g.name),

    name=Identifier(Token.Identifier),
    literal=Literal(Token.Number),
)
build_and_run(g, 'main.py')
print('Done')
