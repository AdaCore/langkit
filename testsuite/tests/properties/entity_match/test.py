"""
Check that match expression on entity types works properly.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T
from langkit.expressions import AbstractProperty, Property, Self
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    get_num = AbstractProperty(T.LongType)


class Example(FooNode):
    get_num = Property(2)


class Literal(FooNode):
    tok = Field()

    get_num = Property(3)

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)

    b = Property(
        Self.a.match(
            lambda e=Example.entity: e.get_num,
            lambda c=FooNode.entity: c.get_num,
        ),
        public=True
    )

    c = Property(
        Self.a.match(
            lambda e=Example: e.get_num,
            lambda c=FooNode: c.get_num,
        ),
        public=True
    )


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(foo_grammar.literal, foo_grammar.example),
    literal=Literal(Tok(Token.Number, keep=True)),
    example=Example('example'),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
