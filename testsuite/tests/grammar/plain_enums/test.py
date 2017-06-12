"""
Test that LogiVarType bindings in the Python API are properly working.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    ASTNode, BoolType, EnumType, Field, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.parsers import Enum, Grammar, List, Opt, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class MyEnum(EnumType):
    alternatives = ['e_example', 'e_null']


class EnumNode(FooNode):
    my_enum = Field(type=MyEnum)
    has_plus = Field(type=BoolType)


class Nodes(FooNode):
    nodes = Field(type=EnumNode.list_type())


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Nodes(List(EnumNode(
        Or(Enum(Tok(Token.Example), MyEnum('e_example')),
           Enum(Tok(Token.Null), MyEnum('e_null'))),
        Opt('+').as_bool()
    ))),
)

build_and_run(foo_grammar, 'main.py')

print('Done')
