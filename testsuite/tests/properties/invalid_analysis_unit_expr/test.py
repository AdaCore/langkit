"""
Test that invalid uses of analysis unit in the properties DSL are properly
detected and reported.
"""

from langkit.dsl import ASTNode
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


def run(name, prop):
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        result = Property(prop)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run('Invalid field', Self.unit.foobar)
print('Done')
