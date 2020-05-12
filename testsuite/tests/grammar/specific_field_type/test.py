"""
Test that a warning is emitted when the type of a parsing field is not as
specific as it could be.
"""

from langkit.dsl import ASTNode, Field

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleWrapper(FooNode):
    example = Field(type=FooNode)


class Example(FooNode):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
