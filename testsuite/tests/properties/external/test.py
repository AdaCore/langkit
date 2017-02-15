"""
Test that external properties build and run properly.
"""

import os.path

from langkit.compiled_types import (
    ASTNode, Field, LongType, abstract, root_grammar_class
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
    result = AbstractProperty(type=LongType, public=True)


class Literal(Expression):
    tok = Field()

    result = ExternalProperty()


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
    atom=Row(Tok(Token.Number, keep=True)) ^ Literal,
)
build_and_run(foo_grammar, 'main.py')
print 'Done'
