"""
Test that property checks are properly emitted when null checks should trigger
them.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import AnalysisUnit, ASTNode, Field, T, abstract
from langkit.expressions import No, Property, Self
from langkit.parsers import Grammar, Or, Pick

from lexer_example import Token
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


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=foo_grammar.expression,
    expression=Or(
        Pick('(', foo_grammar.expression, ')'),
        Plus(foo_grammar.atom, '+', foo_grammar.main_rule),
        foo_grammar.atom,
    ),
    atom=Or(
        Literal(Token.Number),
        Name(Token.Identifier),
    ),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
