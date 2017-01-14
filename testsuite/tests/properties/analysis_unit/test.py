"""
Test the handling of analysis units in the properties DSL.
"""

import os.path

from langkit.compiled_types import (
    AnalysisUnitType, ASTNode, Field, LongType, abstract, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import (
    AbstractProperty, ExternalProperty, Property, Self
)
from langkit.parsers import Grammar, Or, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


@abstract
class Expression(FooNode):
    result = AbstractProperty(type=LongType)


class Literal(Expression):
    tok = Field()

    result = ExternalProperty()


class Name(Expression):
    tok = Field()

    designated_unit = ExternalProperty(type=AnalysisUnitType)
    result = Property(Self.designated_unit.root.cast(Expression).result)


class Plus(Expression):
    left = Field()
    right = Field()

    result = Property(Self.left.result + Self.right.result)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(
        Row(foo_grammar.atom, '+', foo_grammar.main_rule) ^ Plus,
        foo_grammar.atom
    ),
    atom=Or(
        Row(Tok(Token.Number, keep=True)) ^ Literal,
        Row(Tok(Token.Identifier, keep=True)) ^ Name,
    ),
)
build_and_run(foo_grammar, 'main.py')
print 'Done'
