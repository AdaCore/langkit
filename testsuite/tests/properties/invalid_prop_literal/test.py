from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Property
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(lit):
    class FooNode(ASTNode):
        pass

    class ExampleNode(FooNode):
        b = Property(lit, public=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=ExampleNode('example'))
    emit_and_print_errors(grammar)

print('Valid case')
run(12)
print('Valid case')
run('lol')
print('Invalid case')
run(12.90)
print('')
print('Done')
