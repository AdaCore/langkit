"""
Tests that it is possible to call a property of type T on an Entity[T].
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    ASTNode, Field, root_grammar_class, LongType, abstract
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import AbstractProperty, Property, Self
from langkit.parsers import Grammar, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    prop = AbstractProperty(runtime_check=True, type=LongType)


@abstract
class BarNode(FooNode):
    pass


class Literal(FooNode):
    tok = Field()

    a = AbstractProperty(runtime_check=True, type=FooNode.entity())

    b = Property(
        Self.a.match(
            lambda b=BarNode.entity(): b.prop,
            lambda c=FooNode.entity(): c.prop,
        ),
        public=True
    )


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Row(Tok(Token.Number, keep=True)) ^ Literal,
)
build_and_run(foo_grammar, 'main.py')
print('Done')
