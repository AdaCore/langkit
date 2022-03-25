"""
Check that ``NPropagate`` expressions work as expected.
"""

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.expressions import (
    All, Bind, Entity, If, NPropagate, PropertyError, Self, Var, ignore,
    langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    v = UserField(type=T.LogicVar, public=False)


class Literal(FooNode):
    token_node = True

    @langkit_property()
    def combiner(other=T.Literal.entity):
        ignore(other)
        return Entity


class Plus(FooNode):
    lhs = Field()
    rhs = Field()

    @langkit_property(public=True)
    def resolve(lhs=T.FooNode.entity, rhs=T.FooNode.entity):
        eq = Var(
            All([
                NPropagate(Self.v, T.Literal.combiner, Self.lhs.v, Self.rhs.v),
                Bind(Self.lhs.v, lhs),
                Bind(Self.rhs.v, rhs),
            ])
        )
        return If(
            eq.solve,
            Self.v.get_value,
            PropertyError(T.FooNode.entity, "unreachable")
        )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
