from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit.compiled_types import (
    ASTNode, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Literal, Property, Let
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in ExampleNode.
    """

    global Compound, Expression, FooNode, NullNode, Number

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    @root_grammar_class()
    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        prop = Property(expr, public=True)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Row('example') ^ BarNode,
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run("Correct code", lambda: Let(lambda a=Literal(1): a))
run("Missing var value", lambda: Let(lambda a: a))
run("Invalid args", lambda: Let(lambda a=Literal(1), *b, **c: a))

print('Done')
