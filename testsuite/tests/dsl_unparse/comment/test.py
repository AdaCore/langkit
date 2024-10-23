from langkit.dsl import ASTNode

from utils import emit_and_print_errors, unparse_all_script


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file="expected_concrete_syntax.lkt", unparse_script=unparse_all_script
)
print('Done')
