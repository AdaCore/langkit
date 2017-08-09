from __future__ import absolute_import, division, print_function

from os import path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


class ListNode(FooNode):
    nb_list = Field()
    prop = Property(Self.nb_list.map(lambda i, _: i), public=True)


class NumberNode(FooNode):
    tok = Field()


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=ListNode(
        List(NumberNode(Tok(Token.Number, keep=True)))
    ),
)


build_and_run(foo_grammar, 'main.py')
print('Done')
