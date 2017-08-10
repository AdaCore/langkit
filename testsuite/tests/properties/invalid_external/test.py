from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, T, abstract
from langkit.expressions import ExternalProperty, Property, Self
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


def run(name, abstract_prop, prop=None):
    """
    Emit and print the errors we get for the below grammar with `abstract_prop`
    as a property in AbstractExample and `prop` (if provided) as a property in
    Example.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    @abstract
    class AbstractExample(FooNode):
        p = abstract_prop()

    class Example(AbstractExample):
        if prop:
            p = prop()

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Row('example') ^ Example,
    )
    emit_and_print_errors(grammar)
    print('')


run('Missing type',
    lambda: ExternalProperty(uses_entity_info=False))

run('Invalid abstract',
    lambda: ExternalProperty(abstract=True, type=T.FooNode,
                             uses_entity_info=False),
    lambda: Property(Self))

run('Invalid memoized',
    lambda: ExternalProperty(memoized=True, type=BoolType,
                             uses_entity_info=False))
print('Done')
