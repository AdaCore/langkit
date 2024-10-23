from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import AbstractKind, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(
        kind=AbstractKind.abstract_runtime_check,
        return_type=T.Bool,
        public=True,
    )
    def p(i=T.Int):
        pass


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file="expected_concrete_syntax.lkt", unparse_script=unparse_script
)
print("Done")
