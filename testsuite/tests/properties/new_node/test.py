"""
Test AST node synthetization and a basic use of it in the Python API.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import ASTNode, Field, Token, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import New, Property, Self
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token as LexToken
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Literal(FooNode):
    tok = Field()


class SynthNode(FooNode):
    name = Field(type=Token)
    items = Field(type=Literal.list_type())


class LiteralSequence(FooNode):
    name = Field()
    items = Field()

    prop = Property(
        New(SynthNode, name=Self.name, items=Self.items),
        public=True,
        memoized=True
    )


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=foo_grammar.list_rule,
    list_rule=Row('(',
                  Tok(LexToken.Identifier, keep=True),
                  List(foo_grammar.list_item, sep=','),
                  ')') ^ LiteralSequence,
    list_item=Row(Tok(LexToken.Number, keep=True)) ^ Literal,
)
build_and_run(foo_grammar, 'main.py')
print('Done')
