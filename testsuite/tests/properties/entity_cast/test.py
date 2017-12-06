"""
Test that cast expressions work on entity prefixes.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.expressions import AbstractProperty, Property, Self
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    pass


class Literal(FooNode):
    tok = Field()

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)

    b = Property(Self.a.cast(BarNode.entity))

    c = Property(Self.b, public=True)

    d = Property(Self.a.cast(BarNode),
                 type=BarNode.entity,
                 public=True)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(Literal(Tok(Token.Number, keep=True)),
                 BarNode('example')),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
