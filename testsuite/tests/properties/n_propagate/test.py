"""
Check that ``NPropagate`` expressions work as expected.
"""

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.expressions import (
    And, ArrayLiteral, Bind, If, NPropagate, PropertyError, Self, Var,
    langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    v = UserField(type=T.LogicVar, public=False)


class Literal(FooNode):
    token_node = True

    @langkit_property()
    def static_combiner(other=T.Literal.entity):
        return other

    @langkit_property()
    def dynamic_combiner(lits=T.Literal.entity.array):
        return lits.at(0)


class Plus(FooNode):
    lhs = Field()
    rhs = Field()

    @langkit_property(public=True)
    def resolve(lhs=T.FooNode.entity,
                rhs=T.FooNode.entity,
                use_dynamic_combiner=T.Bool):
        arr = Var(ArrayLiteral([Self.lhs.v, Self.rhs.v]))
        propagator = Var(If(
            use_dynamic_combiner,
            NPropagate(Self.v, T.Literal.dynamic_combiner,
                       ArrayLiteral([Self.lhs.v, Self.rhs.v]))
            # Check that array passed to the dynamic combiner need not be
            # an array literal.
            & NPropagate(Self.v, T.Literal.dynamic_combiner, arr),
            NPropagate(Self.v, T.Literal.static_combiner,
                       Self.lhs.v, Self.rhs.v),
        ))
        eq = Var(And(
            propagator,
            Bind(Self.lhs.v, lhs),
            Bind(Self.rhs.v, rhs),
        ))
        return If(
            eq.solve,
            Self.v.get_value,
            PropertyError(T.FooNode.entity, "unreachable")
        )


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
