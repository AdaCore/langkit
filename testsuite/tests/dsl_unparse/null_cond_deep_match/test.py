from langkit.dsl import ASTNode, T
from langkit.expressions import Entity, No, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p1(n=T.FooNode.entity):
        return n._.match(
            lambda e=T.Example: e,
            lambda _: No(T.FooNode.entity),
        ).then(lambda n: n.parent)

    @langkit_property(public=True)
    def p2():
        return (
            Entity.to_singleton
            .find(
                lambda e: e.is_a(T.Example)
            )._.match(
                lambda e=T.Example: e.parent,
                lambda _: No(T.FooNode.entity),
            )._.cast(T.Example)
        )

    @langkit_property()
    def to_singleton():
        return [Entity]


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
