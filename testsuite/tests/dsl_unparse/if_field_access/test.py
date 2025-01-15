from langkit.dsl import ASTNode, T
from langkit.expressions import If, No, Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p():
        return If(
            Self.is_a(T.Example),
            Self,
            No(T.Example)
        ).cast(T.FooNode).to_entity

    @langkit_property()
    def to_entity():
        return Self.as_bare_entity


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
