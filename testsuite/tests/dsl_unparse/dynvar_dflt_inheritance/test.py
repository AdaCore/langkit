from langkit.dsl import ASTNode, T
from langkit.expressions import DynamicVariable, langkit_property

from utils import emit_and_print_errors, unparse_script


v = DynamicVariable('v', type=T.Bool)


class FooNode(ASTNode):

    @langkit_property(public=True, dynamic_vars=[(v, False)])
    def p():
        return True


class Example(FooNode):
    token_node = True

    @langkit_property()
    def p():
        return False


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_script,
    types_from_lkt=False,
)
print('Done')
