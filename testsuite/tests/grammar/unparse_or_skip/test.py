"""
Test the generation of unparsers involving Or parsers that contain Skip parsers
as alternatives.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Or, Skip

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class DefNode(FooNode):
    name = Field()
    value = Field()


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


class ErrorDef(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(Or(
        DefNode('def', g.identifier, '=', g.number, ';'),
        Skip(ErrorDef))
    ),

    number=Number(Token.Number),
    identifier=Identifier(Token.Identifier),
)
build_and_run(g, ada_main='main.adb', generate_unparser=True)

print('Done')
