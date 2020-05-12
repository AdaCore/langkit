"""
Test that the unparsing machinery rejects lexers with Ignore actions.
"""

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt', generate_unparser=True)
print('Done')
