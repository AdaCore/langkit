from langkit.compiled_types import (
    ASTNode, root_grammar_class, LongType
)
from langkit.diagnostics import LangSourceDir
from langkit.expressions import Property, No
from langkit.parsers import Grammar, Row

from lexer_example import foo_lexer
from os import path
from utils import emit_and_print_errors, reset_langkit


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode

    LangSourceDir.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))
    reset_langkit()

    @root_grammar_class
    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        prop = Property(expr)

    foo_grammar = Grammar()
    foo_grammar.main_rule_name = 'main_rule'
    foo_grammar.add_rules(
        main_rule=Row('example') ^ BarNode,
    )
    emit_and_print_errors(foo_grammar)
    print('')


run("Correct code", lambda: No(FooNode))
run("Incorrect No usage", lambda: No(LongType))
print 'Done'
