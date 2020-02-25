"""
Check that property memoization is properly rejected when using unsupported
argument types.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, memoized=True)
    def prop(a=T.Token.array):
        return a.length == 0


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
