"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import ASTNode, Field, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, List, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Enum(T.EnumNode):
    alternatives = ['null', 'example', 'default']


class Param(FooNode):
    name = Field(type=T.Name)
    mode = Field(type=T.Enum)


class Name (FooNode):
    tok = Field(type=T.Token)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(Param(foo_grammar.name, foo_grammar.mode)),
    name=Name(Tok(Token.Identifier, keep=True)),
    mode=Or(
        Enum.alt_null('null'),
        Enum.alt_example('example'),
        Enum.alt_default(),
    ),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
