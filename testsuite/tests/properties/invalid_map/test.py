from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.expressions import Entity, Property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode

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
        main_rule=BarNode('example', grammar.list_rule),
        list_rule=ListNode(
            List(NumberNode(Token.Number))
        ),
    )
    emit_and_print_errors(grammar)
    print('')


run("Correct code", lambda: Entity.nb_list.map(lambda x: x))
run("Incorrect map code 1", lambda: Entity.nb_list.map(lambda x, y, z: x))
run("Incorrect map code 2", lambda: Entity.nb_list.map(lambda x=12: x))
run("Incorrect map code 3", lambda: Entity.nb_list.map(lambda x, *y: x))
print('Done')
