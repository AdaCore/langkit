from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class ListNode(FooNode):
    nb_list = Field()
    prop = Property(Self.nb_list.map(lambda i, _: i), public=True)


class NumberNode(FooNode):
    token_node = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=ListNode(
        List(NumberNode(Token.Number))
    ),
)


build_and_run(foo_grammar, 'main.py')
print('Done')
