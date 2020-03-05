"""
Test that the unparsing machinery rejects lexers with Ignore actions.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt', generate_unparser=True)
print('Done')
