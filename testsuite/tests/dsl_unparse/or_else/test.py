from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p():
        return Self.cast(T.Example).parent._or(Self).parent


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_script,
    types_from_lkt=False,
)
print('Done')
