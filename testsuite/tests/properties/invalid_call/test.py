from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, LongType
from langkit.expressions import Property, No, Self
from langkit.parsers import Grammar, Row

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
        prop_2 = Property(lambda x=LongType: x, public=True)
        prop = Property(expr, public=True)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Row('example') ^ BarNode,
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run("Correct code", lambda: Self.prop_2(12))
run("Incorrect call code 1", lambda: Self.non_exisisting_prop(12))
run("Incorrect call code 2", lambda: Self.prop_2(12, 15))
run("Incorrect call code 3", lambda: Self.prop_2(No(FooNode)))
print('Done')
