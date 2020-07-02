"""
Check that printing diagnostics (in particular quoting the source buffer) works
fine when the source buffer is empty.
"""

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='input.lkt')
print('Done')
