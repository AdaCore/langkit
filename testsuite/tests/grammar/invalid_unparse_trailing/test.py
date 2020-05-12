"""
Test that inconsistencies in regular node postfix parsers are properly
reported.
"""

from langkit.dsl import ASTNode, Field

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Assignment(FooNode):
    name = Field()
    value = Field()


class Decl(FooNode):
    assignment = Field()
    example = Field()


class Example(FooNode):
    token_node = True


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt', generate_unparser=True)
print('Done')
