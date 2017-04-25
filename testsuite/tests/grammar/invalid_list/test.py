from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import ASTNode, Field, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from utils import emit_and_print_errors, reset_langkit


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


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


emit_and_print_errors(lang_def)
reset_langkit()
emit_and_print_errors(lang_def_2)
print('Done')
