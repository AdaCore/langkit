from langkit.compiled_types import (
    ASTNode, root_grammar_class, LongType, Field
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, No, Self
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors, reset_langkit


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))
    reset_langkit()

    @root_grammar_class
    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        list_node = Field()

    class ListNode(FooNode):
        nb_list = Field()
        prop = Property(expr)

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row('example', foo_grammar.list_rule) ^ BarNode,
        list_rule=Row(List(Tok(Token.Number))) ^ ListNode,
    )
    emit_and_print_errors(foo_grammar)
    print('')


run("Correct code", lambda: Self.nb_list.map(lambda x: x))
run("Incorrect map code 1", lambda: Self.nb_list.map(lambda x, y, z: x))
run("Incorrect map code 2", lambda: Self.nb_list.map(lambda x=12: x))
run("Incorrect map code 3", lambda: Self.nb_list.map(lambda x, *y: x))
print 'Done'
