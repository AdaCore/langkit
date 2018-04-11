"""
Test the generation of unparsers in a configuration where there is no single
cannonical parser that contains all information for unparsing. As a
consequence, this also checks that unparsers are properly combined.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Null, Opt, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class RootNode(FooNode):
    ident = Field()
    number = Field()


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(main_rule=List(Or(
    RootNode('def',
             Null(Identifier),
             Opt('{', Number(Token.Number), '}'),
             ';'),
    RootNode('def',
             Opt('(', Identifier(Token.Identifier), ')'),
             Null(Number),
             ';'),
)))
build_and_run(g, ada_main='main.adb', generate_unparser=True)

print('Done')
