"""
Test that inconsistencies in regular node postfix parsers are properly
reported.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Assignment(FooNode):
    name = Field()
    value = Field()


class Decl(FooNode):
    assignment = Field()
    example = Field()


class Example(FooNode):
    token_node = True


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(Or(g.decl, g.assignment)),

    assignment=Assignment(g.identifier, '=', g.number, ';'),

    decl=Decl('def', g.decl_assignment, ',', Example('example')),
    decl_assignment=Assignment(g.identifier, '=', g.number),

    identifier=Identifier(Token.Identifier),
    number=Number(Token.Number),
)
emit_and_print_errors(g, generate_unparser=True)

print('Done')
