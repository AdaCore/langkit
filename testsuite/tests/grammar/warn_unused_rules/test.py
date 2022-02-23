"""
Check that the warning for unused parsing rules works as expected.
"""

from langkit.dsl import ASTNode, Field, abstract

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    pass


class VarDecl(Decl):
    name = Field()


class FunDecl(Decl):
    name = Field()


class Name(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


class Example(FooNode):
    num = Field()


class NullDecl(Decl):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
