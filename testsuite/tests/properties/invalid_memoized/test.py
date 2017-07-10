from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


def run(name, prop):
    """
    Emit and print the errors we get for the below grammar with `prop()` as
    a property in Example.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        if prop:
            p = prop()

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Row('example') ^ Example,
        )
        return foo_grammar
    emit_and_print_errors(lang_def)
    print('')


run('Invalid uses entity info',
    lambda: Property(Self.as_entity, memoized=True))

print('Done')
