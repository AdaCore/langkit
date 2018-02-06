"""
Check that map expressions on entity types work properly.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.expressions import Entity, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Names(FooNode):
    names = Field()

    @langkit_property(public=True)
    def count():
        return Entity.names.length


class Name(FooNode):
    token_node = True


fg = Grammar('main_rule')
fg.add_rules(
    main_rule=Names(List(Name(Token.Identifier))),
)
build_and_run(fg, 'main.py')
print('Done')
