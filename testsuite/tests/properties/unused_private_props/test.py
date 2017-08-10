"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import AnalysisUnitType, ASTNode, Field, LongType, T, abstract
from langkit.expressions import (
    AbstractProperty, EmptyArray, ExternalProperty, Property, Self,
    langkit_property
)
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    # This property is private and only called by unused properties, so it is
    # unused itself.
    as_expr = Property(Self.cast(T.Expression))

    # This property is unused but the user asked explicitly to not warn
    @langkit_property(warn_on_unused=False)
    def as_expr_2():
        return Self.cast(T.Expression)


@abstract
class Expression(FooNode):
    # This property and all its children are private. Only Literal.result is
    # called by a public property, so all others are unused.
    result = AbstractProperty(type=LongType)

    # This property is private, but is called from "referenced_units", so
    # "names" and all its overriding properties are used.
    names = AbstractProperty(type=T.Name.array)

    referenced_units = Property(Self.names.map(lambda n: n.designated_unit),
                                public=True)


class Literal(Expression):
    tok = Field()

    # This one is private, but it is called by "evaluate" so it's not usused
    result = ExternalProperty(uses_entity_info=False)

    # See Expression.name
    names = Property(EmptyArray(T.Name))

    evaluate = Property(Self.result, public=True)


class Name(Expression):
    tok = Field()

    # This one is private and called transitively from a public property
    designated_unit = ExternalProperty(type=AnalysisUnitType,
                                       uses_entity_info=False)

    result = Property(Self.designated_unit.root.cast(Expression).result)

    # See Expression.name
    names = Property(Self.singleton)


class Plus(Expression):
    left = Field()
    right = Field()

    result = Property(Self.left.result + Self.right.result)

    # See Expression.name
    names = Property(Self.left.names.concat(Self.right.names))


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Or(
        Plus(grammar.atom, '+', grammar.main_rule),
        grammar.atom
    ),
    atom=Or(
        Literal(Tok(Token.Number, keep=True)),
        Name(Tok(Token.Identifier, keep=True)),
    ),
)
emit_and_print_errors(grammar)
print('Done')
