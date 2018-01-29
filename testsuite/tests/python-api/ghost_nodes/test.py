"""
Test that the "is_ghost" AST node predicate works in the Python API.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, EnumNode, Field, T
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Enum(EnumNode):
    alternatives = ['null', 'example', 'default']


class PlusQualifier(EnumNode):
    qualifier = True


class Param(FooNode):
    name = Field(type=T.Name)
    mode = Field(type=T.Enum)
    has_plus = Field(type=T.PlusQualifier)


class Name (FooNode):
    token_node = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(Param(foo_grammar.name,
                         foo_grammar.mode,
                         foo_grammar.plus)),
    name=Name(Token.Identifier),
    mode=Or(
        Enum.alt_null('null'),
        Enum.alt_example('example'),
        Enum.alt_default(),
    ),
    plus=PlusQualifier('+'),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
