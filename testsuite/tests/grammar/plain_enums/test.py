"""
Test that LogiVarType bindings in the Python API are properly working.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, Field, EnumType
from langkit.parsers import Enum, Grammar, List, Opt, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class MyEnum1(EnumType):
    alternatives = ['e_example', 'e_null']


class MyEnum2(EnumType):
    """
    Documented enum.

    This enumeration type is documented, and thus tests documentation
    generation for it.
    """
    alternatives = ['e_example', 'e_null']


class EnumNode(FooNode):
    enum_1 = Field(type=MyEnum1)
    enum_2 = Field(type=MyEnum2)
    has_plus = Field(type=BoolType)


class Nodes(FooNode):
    nodes = Field(type=EnumNode.list)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Nodes(List(EnumNode(
        Or(Enum(Tok(Token.Example), MyEnum1('e_example')),
           Enum(Tok(Token.Null), MyEnum1('e_null'))),
        Enum(Tok(Token.Example), MyEnum2('e_example')),
        Opt('+').as_bool()
    ))),
)

build_and_run(foo_grammar, 'main.py')

print('Done')
