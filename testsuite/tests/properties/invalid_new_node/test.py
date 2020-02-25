from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, synthetic
from langkit.expressions import New, Property

from utils import emit_and_print_errors


def run(name, prop_fn, prop_memoized):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Literal(FooNode):
        token_node = True

    @synthetic
    class EmptyNode(FooNode):
        pass

    @synthetic
    class LiteralList(Literal.list):
        prop = Property(prop_fn(), memoized=prop_memoized)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run('Not memoized', lambda: New(T.EmptyNode), False)
run('List synthetization', lambda: New(T.LiteralList), True)
run('Not synthetic', lambda: New(T.Literal), True)
print('Done')
