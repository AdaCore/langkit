from langkit.dsl import ASTNode, T
from langkit.expressions import Entity, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p():
        return (
            Entity.singleton._.find(
                lambda n: n.cast(T.Example).then(
                    lambda e: e.parent.is_null
                )
            )
            .then(lambda n: n.parent)
        )


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
