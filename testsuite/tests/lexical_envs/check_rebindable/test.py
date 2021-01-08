"""
Check that one cannot rebind using an environment whose node isn't tagged as
rebindable.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Annotations, Field, abstract
from langkit.envs import EnvSpec, add_env
from langkit.expressions import No, Self, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class BaseEnvCreator(FooNode):
    env_spec = EnvSpec(add_env())


class DefaultEnvCreator(BaseEnvCreator):
    pass


class RebindableEnvCreator(BaseEnvCreator):
    annotations = Annotations(rebindable=True)


class Rebinder(FooNode):
    left = Field(type=BaseEnvCreator)
    right = Field(type=BaseEnvCreator)

    @langkit_property(public=True)
    def rebind():
        left_env = Var(Self.left.children_env)
        right_env = Var(Self.right.children_env)

        # Depending on whether "left_env" is rebindable, the following will
        # succeed or raise a PropertyError. The status of "right_env" does not
        # matter.
        r = Var(No(T.EnvRebindings).append_rebinding(left_env, right_env))

        return Self.children_env.rebind_env(r).env_node

    env_spec = EnvSpec(add_env())


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
