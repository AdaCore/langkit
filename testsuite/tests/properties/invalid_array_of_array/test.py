from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    array_of_nodes = Property(Self.singleton, public=True)
    array_of_array = Property(Self.singleton.singleton, public=True)


emit_and_print_errors(lkt_file='foo.lkt')
print('')
print('Done')
