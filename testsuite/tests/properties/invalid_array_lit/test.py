from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import ArrayLiteral, Property, Self

from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in Example.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        p = Property(expr)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run('Empty: missing type', [])
run('Empty: ok', ArrayLiteral([], T.Bool))
run('Single: ok', [True])
run('Multiple: heterogeneous types', [True, Self])
print('Done')
