"""
Check that invalid inputs for the ".symbol" DSL attribute are properly
rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import Property, Self

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
        token_node = True
        p = Property(expr.symbol, public=True)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run('Valid   - Token node', Self)
run('Invalid - Bad type', Self.is_null)
run('Invalid - Non-token node', Self.cast(T.FooNode))
print('Done')
