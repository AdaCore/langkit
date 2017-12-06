"""
Test that invalid uses of analysis unit in the properties DSL are properly
detected and reported.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Property, Self
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, prop):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        result = Property(prop)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Example('example'))
    emit_and_print_errors(grammar)
    print('')


run('Invalid field', Self.unit.foobar)
print('Done')
