from __future__ import absolute_import, division, print_function

import langkit
from langkit.dsl import ASTNode, Annotations, Field
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import emit_and_print_errors


def create_nodes():
    global FooNode, ListNode, Num

    class FooNode(ASTNode):
        annotations = Annotations(generic_list_type='FooList')

    class ListNode(FooNode):
        items = Field()

    class Num(FooNode):
        field = Field()


create_nodes()
grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=ListNode(List(Token.Number)),
)
emit_and_print_errors(grammar)

langkit.reset()
create_nodes()
grammar = Grammar('main_rule')
grammar.add_rules(
    num=Num(Token.Number),
    main_rule=List(grammar.num, list_cls=ListNode)
)
emit_and_print_errors(grammar)

print('Done')
