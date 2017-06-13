"""
Test that Struct types bindings in the Python API are properly working.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import (ASTNode, BoolType, Field, Struct, T,
                         Token as TokenType, root_grammar_class)
from langkit.expressions import langkit_property
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class Thing(Struct):
    node = Field(type=T.Example)
    comes_from_source = Field(type=BoolType)


class Example(FooNode):
    tok = Field(type=TokenType)

    @langkit_property(public=True, return_type=Thing)
    def entity_id(e=Thing):
        return e


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Identifier, keep=True)),
)

build_and_run(foo_grammar, 'main.py', library_fields_all_public=True)
print('Done')
