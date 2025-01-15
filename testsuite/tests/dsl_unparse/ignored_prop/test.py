from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(return_type=T.Bool, warn_on_unused=False)
    def p1():
        return False

    @langkit_property(return_type=T.Bool, warn_on_unused=False)
    def p2():
        return False

    @langkit_property(return_type=T.Bool)
    def p3():
        return False


class Example(FooNode):
    token_node = True

    @langkit_property(return_type=T.Bool)
    def p2():
        return False

    @langkit_property(return_type=T.Bool, warn_on_unused=False)
    def p3():
        return False


emit_and_print_errors(
    lkt_file="expected_concrete_syntax.lkt", unparse_script=unparse_script
)
print("Done")
