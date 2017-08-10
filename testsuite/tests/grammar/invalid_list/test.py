from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, Annotations
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from utils import emit_and_print_errors, reset_langkit


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


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
    main_rule=Row(List(Tok(Token.Number, keep=True))) ^ ListNode,
)
emit_and_print_errors(grammar)

reset_langkit()
create_nodes()
grammar = Grammar('main_rule')
grammar.add_rules(
    num=Num(Tok(Token.Number, keep=True)),
    main_rule=List(grammar.num, list_cls=ListNode)
)
emit_and_print_errors(grammar)

print('Done')
