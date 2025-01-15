from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p():
        return Self.parents.filter(lambda n: n.parent.is_null).at(0)._.match(
            lambda e=T.Example: e,
            lambda n: n,
        )


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
