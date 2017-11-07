from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, T
from langkit.expressions import Property, Self
from langkit.parsers import Grammar

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

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example'),
    )
    emit_and_print_errors(grammar)
    print('')


run('Invalid uses entity info',
    lambda: Property(lambda a=T.entity.array:
                     a.any(lambda item: item == Self.as_entity),
                     memoized=True))

print('Done')
