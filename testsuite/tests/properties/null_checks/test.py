"""
Test that property checks are properly emitted when null checks should trigger
them.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    AnalysisUnitType, ASTNode, Field, T, Token as TokenType, abstract,
    root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import No, Property, Self
from langkit.parsers import Grammar, Or, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    null_unit = Property(No(AnalysisUnitType), public=True)
    null_node = Property(No(T.Expression), public=True)

    deref_null_unit = Property(Self.null_unit.root, public=True)
    deref_null_node = Property(Self.null_node.null_node, public=True)
    null_node_unit = Property(Self.null_node.unit, public=True)

    cast_null_node = Property(Self.null_node.cast(T.Name), public=True)

    match_null_node = Property(
        Self.null_node.match(
            lambda l=T.Literal: l,
            lambda n=T.Name: n,
            lambda others: others
        ),
        public=True
    )


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    tok = Field(type=TokenType)


class Name(Expression):
    tok = Field(type=TokenType)

    env_element = Property(Self.children_env.get(Self.tok.symbol).at(0))
    deref_env_element = Property(Self.env_element.null_node, public=True)
    match_env_element = Property(
        Self.env_element.match(
            lambda l=T.Literal.entity(): l.el,
            lambda n=T.Name.entity(): n.el,
            lambda others: others.el
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
        Row('(', foo_grammar.expression, ')')[1],
        Plus(foo_grammar.atom, '+', foo_grammar.main_rule),
        foo_grammar.atom,
    ),
    atom=Or(
        Row(Tok(Token.Number, keep=True)) ^ Literal,
        Row(Tok(Token.Identifier, keep=True)) ^ Name,
    ),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
