"""
Test that nodes that contain empty lists with no token associated are properly
unparsed.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Null

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class DefNode(FooNode):
    name = Field()
    values = Field()


class Identifier(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(main_rule=List(g.def_node),
            def_node=DefNode('(', g.identifier, ',',
                             Null(DefNode.list), ')'),
            identifier=Identifier(Token.Identifier))
build_and_run(g, ada_main='main.adb', generate_unparser=True)

print('Done')
