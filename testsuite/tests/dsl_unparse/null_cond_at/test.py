from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p1():
        return Self.p2._.at(0).cast(T.Example)

    @langkit_property()
    def p2():
        return [Self]


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
