"""
Check that invalid token literals in the grammar are properly reported.
"""

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
