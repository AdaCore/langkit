from langkit.dsl import ASTNode, T
from langkit.expressions import If, Self, Var, langkit_property, named_env

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    @langkit_property(public=True)
    def p1():
        dest_env = Var(named_env(
            Self.p2(False),
            or_current=True,
        ))
        return dest_env.env_name

    @langkit_property()
    def p2(b=T.Bool):
        return If(b, "foo", "bar")


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_script,
    types_from_lkt=False,
)
print('Done')
