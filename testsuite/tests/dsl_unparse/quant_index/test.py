"""
Check the unparsing of quantifier expressions with a lambda argument for the
iteration index.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Bool)
    def match_formal_params():
        return Self.children.all(lambda i, p: (i == 0) & (p == Self))


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file="expected_concrete_syntax.lkt", unparse_script=unparse_script
)
print("Done")
