from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from utils import emit_and_print_errors, reset_langkit


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


def create_nodes():
    global FooNode, ListNode, Num

    class FooNode(ASTNode):
        _generic_list_type = 'FooList'

    class ListNode(FooNode):
        items = Field()

    class Num(FooNode):
        field = Field()


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row(List(Tok(Token.Number, keep=True))) ^ ListNode,
    )
    return foo_grammar


def lang_def_2():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        num=Num(Tok(Token.Number, keep=True)),
        main_rule=List(foo_grammar.num, list_cls=ListNode)
    )
    return foo_grammar


create_nodes()
emit_and_print_errors(lang_def)

reset_langkit()
create_nodes()
emit_and_print_errors(lang_def_2)
print('Done')
