"""
Test that property checks are properly emitted when null checks should trigger
them.
"""

from langkit.dsl import ASTNode, AnalysisUnit, Field, T, abstract
from langkit.expressions import No, Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    null_unit = Property(No(AnalysisUnit), public=True)
    null_node = Property(No(T.Expression.entity), public=True)

    deref_null_unit = Property(Self.null_unit.root.as_bare_entity, public=True)
    deref_null_node = Property(Self.null_node.null_node,
                               public=True)
    null_node_unit = Property(Self.null_node.unit, public=True)

    cast_null_node = Property(Self.null_node.cast(T.Name), public=True)

    match_null_node = Property(
        Self.null_node.node.match(
            lambda l=T.Literal: l,
            lambda n=T.Name: n,
            lambda others: others
        ).as_bare_entity,
        public=True
    )


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    token_node = True


class Name(Expression):
    token_node = True

    env_element = Property(Self.children_env.get(Self.symbol).at(0))
    deref_env_element = Property(Self.env_element.null_node, public=True)
    match_env_element = Property(
        Self.env_element.match(
            lambda l=T.Literal.entity: l,
            lambda n=T.Name.entity: n,
            lambda others: others,
        ),
        public=True
    )


class Plus(Expression):
    left = Field()
    right = Field()


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
