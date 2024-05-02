from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, reference
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors, unparse_script


class FooNode(ASTNode):

    env_spec = EnvSpec(
        reference(
            Self._.singleton,
            through=T.FooNode.resolver,
        ),
    )

    @langkit_property()
    def resolver():
        return Self.children_env


class Example(FooNode):
    token_node = True


emit_and_print_errors(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_script,
    types_from_lkt=False,
)
print('Done')
