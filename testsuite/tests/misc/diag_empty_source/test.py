"""
Test that the "Debug_Text" function in the Ada API raises a Property_Error when
called on a null node.
"""

from langkit.dsl import ASTNode

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='input.lkt')
print('Done')
