"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    ASTNode, Field, T, abstract, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


@abstract
class RootNode(FooNode):
    name = Property(Self.match(
        lambda e=T.Expr: e.name,
        lambda n=T.Name: n
    ))


class Expr(RootNode):
    name = Field()


class Name(RootNode):
    tok = Field()


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Expr(Name(Tok(Token.Identifier, keep=True)))
    )
    return foo_grammar


emit_and_print_errors(lang_def)
print('Done')
