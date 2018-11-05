"""
Check that inputs for .rebindings_* DSL operations are properly validated.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import No, Property, T
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, expr):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        cond = Property(True)
        p = Property(expr)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Example('example'))
    emit_and_print_errors(grammar)
    print('')


run('.old_env: invalid input type',
    No(T.FooNode).old_env)
run('.new_env: invalid input type',
    No(T.FooNode).new_env)
run('.get_parent: invalid input type',
    No(T.FooNode).get_parent)
print('Done')
