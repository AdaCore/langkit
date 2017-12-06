from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, abstract
from langkit.expressions import AbstractProperty
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, runtime_check):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    @abstract
    class BaseNode(FooNode):
        prop = AbstractProperty(T.BoolType, public=True)

    class Example(BaseNode):
        prop = AbstractProperty(T.BoolType, runtime_check=runtime_check)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Example('example'))
    emit_and_print_errors(grammar)
    print('')


run('Abstracts overrides without runtime check', False)
run('Abstracts overrides with runtime check', True)
print('Done')
