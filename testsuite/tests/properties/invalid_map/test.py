from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field
from langkit.expressions import Entity, Property
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        list_node = Field()

    class ListNode(FooNode):
        nb_list = Field()
        prop = Property(expr, public=True)

    class NumberNode(FooNode):
        tok = Field()

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Row('example', grammar.list_rule) ^ BarNode,
        list_rule=Row(
            List(Tok(Token.Number, keep=True) ^ NumberNode)
        ) ^ ListNode,
    )
    emit_and_print_errors(grammar)
    print('')


run("Correct code", lambda: Entity.nb_list.map(lambda x: x))
run("Incorrect map code 1", lambda: Entity.nb_list.map(lambda x, y, z: x))
run("Incorrect map code 2", lambda: Entity.nb_list.map(lambda x=12: x))
run("Incorrect map code 3", lambda: Entity.nb_list.map(lambda x, *y: x))
print('Done')
