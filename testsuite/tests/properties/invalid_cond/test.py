from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Cond, Property, Self
from langkit.parsers import Grammar

from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        cond = Property(True)
        p = Property(expr)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example'),
    )
    emit_and_print_errors(grammar)
    print('')


run('Missing args', Cond())
run('Missing args', Cond(True))
run('Missing last', Cond(Self.cond, Self))
run('Bad condition type', Cond(Self, True, False))
run('Bad return type', Cond(Self.cond, Self, False))
print('Done')
