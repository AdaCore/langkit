from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Property

from utils import emit_and_print_errors


def run(lit):
    class FooNode(ASTNode):
        pass

    class ExampleNode(FooNode):
        b = Property(lit, public=True)

    emit_and_print_errors(lkt_file='foo.lkt')

print('Valid case')
run(12)
print('Valid case')
run('lol')
print('Invalid case')
run(12.90)
print('')
print('Done')
