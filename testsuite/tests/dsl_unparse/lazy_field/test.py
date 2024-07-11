from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property, lazy_field

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p():
        return Self.cast(T.Example).lf


class Example(FooNode):
    token_node = True

    @lazy_field()
    def lf():
        return 42


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_script,
    types_from_lkt=False,
)
print('Done')
