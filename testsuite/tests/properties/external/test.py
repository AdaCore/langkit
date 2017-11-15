"""
Test that external properties build and run properly.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, LongType, abstract
from langkit.expressions import (
    AbstractProperty, ExternalProperty, Property, Self
)
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


@abstract
class Expression(FooNode):
    result = AbstractProperty(type=LongType, public=True)


class Literal(Expression):
    tok = Field()

    result = ExternalProperty(uses_entity_info=False, uses_envs=False)


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
    atom=Literal(Tok(Token.Number, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
