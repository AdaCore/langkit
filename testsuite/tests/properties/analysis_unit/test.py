"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, AnalysisUnit, Field, Int, T, abstract
from langkit.expressions import (AbstractProperty, ExternalProperty, Property,
                                 Self, langkit_property)
from langkit.parsers import Grammar, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True)
    def eval_unit(u=AnalysisUnit):
        return u.root.cast(T.Expression).result


@abstract
class Expression(FooNode):
    result = AbstractProperty(type=Int, public=True)


class Literal(Expression):
    token_node = True

    result = ExternalProperty(uses_entity_info=False, uses_envs=False)


class Name(Expression):
    token_node = True

    designated_unit = ExternalProperty(
        type=AnalysisUnit, uses_entity_info=False, uses_envs=True
    )
    result = Property(Self.designated_unit.root.cast(Expression).result)


class Plus(Expression):
    left = Field()
    right = Field()

    result = Property(Self.left.result + Self.right.result)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(
        Plus(foo_grammar.atom, '+', foo_grammar.main_rule),
        foo_grammar.atom
    ),
    atom=Or(
        Literal(Token.Number),
        Name(Token.Identifier),
    ),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
