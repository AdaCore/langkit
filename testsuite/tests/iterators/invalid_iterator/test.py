"""
Test iteration over a simple iterator from the public APIs.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Int)
    def get_lit():
        return 2

    @langkit_property(public=True, return_type=T.Int.iterator)
    def values_iterator():
        return Self.get_lit.to_iterator


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
