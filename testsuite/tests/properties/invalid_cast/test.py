from langkit.compiled_types import (
    ASTNode, root_grammar_class, LongType, Field
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, No, Self, Cast
from langkit.parsers import Grammar, Row, List, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors, reset_langkit


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode, BarNode, ListNode

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
        bar_node_parent = Property(Self.parent.cast(BarNode))
        prop = Property(expr)

    foo_grammar = Grammar()
    foo_grammar.main_rule_name = 'main_rule'
    foo_grammar.add_rules(
        main_rule=Row('example', foo_grammar.list_rule) ^ BarNode,
        list_rule=Row(List(Tok(Token.Number))) ^ ListNode,
    )
    emit_and_print_errors(foo_grammar)
    print('')


run("Correct code", lambda: Self.parent.cast(BarNode))
run("Invalid cast 1", lambda: Self.parent.cast(LongType))
run("Invalid cast 2", lambda: Self.bar_node_parent.cast(ListNode))
print 'Done'
