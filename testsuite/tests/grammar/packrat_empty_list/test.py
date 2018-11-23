"""
Test that nested trailing empty lists are correctly handled in node creation.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Opt

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Def(FooNode):
    name = Field()
    defs = Field()
    values = Field()


class Values(FooNode):
    items = Field()


class Number(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.def_rule),
    def_rule=Def('def', g.name, Opt(g.def_rule), g.values),

    values=Values(List(Number(Token.Number), empty_valid=True)),

    name=Name(Token.Identifier),
)
build_and_run(g, ada_main='main.adb')
print('Done')
