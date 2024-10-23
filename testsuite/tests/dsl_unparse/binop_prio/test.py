from langkit.dsl import ASTNode, T
from langkit.expressions import Self, Var, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p1(b=T.Bool.array):
        v1 = Var((b.at(0) | b.at(1)) & (b.at(2) | b.at(3)))
        v2 = Var(b.at(0) & b.at(1) & b.at(2))
        v3 = Var((b.at(0) & b.at(1)) | (b.at(2) & b.at(3)))
        return v1 & v2 & v3

    @langkit_property(public=True)
    def p2():
        return (
            Self.is_a(T.Example) & Self.parent.is_null
        ) | Self.parent.parent.is_null

    @langkit_property(public=True)
    def p3():
        return Self.is_a(T.Example) & (
            Self.parent.is_null | Self.parent.parent.is_null
        )


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
