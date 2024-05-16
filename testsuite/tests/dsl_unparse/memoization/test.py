from langkit.dsl import ASTNode
from langkit.expressions import langkit_property, lazy_field

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True, memoized=True)
    def p1():
        return "foo"

    @langkit_property(public=True, memoized=True, call_memoizable=True)
    def p2():
        return "foo"

    @langkit_property(public=True, call_non_memoizable_because="bar")
    def p3():
        return "foo"

    @lazy_field(public=True)
    def lf1():
        return "foo"


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_script,
    types_from_lkt=False,
)
print('Done')
