"""
Test that invalid AST node parse fields are properly rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, Token, synthetic

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


@synthetic
class SynthExample(FooNode):
    f = Field(type=Token)


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
