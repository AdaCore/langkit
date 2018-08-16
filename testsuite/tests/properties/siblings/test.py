"""
Test that the .next_sibling and .previous_sibling properties work as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Tuple(FooNode):
    id = Field()
    nodes = Field()


g = Grammar('main_rule')
g.add_rules(main_rule=g.tuple,
            tuple=Tuple(Name(Token.Identifier),
                        '(', List(g.tuple, empty_valid=True), ')'))
build_and_run(g, 'main.py')
print('Done')
