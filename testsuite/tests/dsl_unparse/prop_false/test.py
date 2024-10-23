from langkit.dsl import ASTNode
from langkit.expressions import Property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    p = Property(False, public=True)


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt', unparse_script=unparse_script
)
print('Done')
