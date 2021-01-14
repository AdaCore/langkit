"""
Test that invalid uses of Skip parsers are properly rejected.
"""

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='test.lkt')
print('Done')
