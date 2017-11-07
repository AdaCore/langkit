from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, LongType
from langkit.expressions import Property, No
from langkit.parsers import Grammar

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
        prop = Property(expr, public=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=BarNode('example'),
    )
    emit_and_print_errors(grammar)
    print('')


run("Correct code", lambda: No(FooNode.entity))
run("Incorrect No usage", lambda: No(LongType))
print('Done')
