"""
Test that warnings about imprecise type annotations for syntax fields.
"""

from langkit.dsl import ASTNode, AbstractField, Field, T, abstract

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    name = AbstractField(T.FooNode)  # Warning on this abstract field


class VarDecl(Decl):
    var_kw = Field(type=T.FooNode)  # Warning on this concrete field
    name = Field(type=T.Name)


class FunDecl(Decl):
    name = Field()


class VarKeyword(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
