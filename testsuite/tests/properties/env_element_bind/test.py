"""
Test that Bind works when binding from env elements.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import (
    ASTNode, Field, root_grammar_class, LongType, UserField, LogicVarType
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import AbstractProperty, Let, Property, Self, Bind
from langkit.parsers import Grammar, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    prop = AbstractProperty(runtime_check=True, type=LongType, public=True)


class BarNode(FooNode):
    pass


class Literal(FooNode):
    tok = Field()

    a = AbstractProperty(runtime_check=True, type=FooNode.entity())
    var = UserField(LogicVarType, public=False)

    b = Property(Bind(Self.var, Self.a))

    public_prop = Property(Let(lambda _=Self.b: Self), public=True)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Row(Tok(Token.Number, keep=True)) ^ Literal,
)
build_and_run(foo_grammar, 'main.py')
print('Done')
