"""
Test that there is no memory leak when a reference resolved aborts with a
property error.
"""

from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, add_env, reference
from langkit.expressions import PropertyError, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property()
    def resolve():
        return PropertyError(T.LexicalEnv)

    env_spec = EnvSpec(
        add_env(),
        reference([Self.cast(FooNode), Self.cast(FooNode)], T.Example.resolve),
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb',
              lkt_semantic_checks=True)
print('Done')
