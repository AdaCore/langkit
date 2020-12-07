"""
Check that node synthetization fails as expected when the Self.Self_Env is a
foreign env.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field, synthetic
from langkit.envs import EnvSpec, add_env, set_initial_env_by_name
from langkit.expressions import No, Self, lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


@synthetic
class SynthNode(FooNode):
    id = Field(type=T.Identifier)


class Identifier(FooNode):
    token_node = True


class DefBlock(FooNode):
    id = Field(type=T.Identifier)

    env_spec = EnvSpec(add_env(names=[Self.id.symbol]))


class RegularBlock(FooNode):
    id = Field(type=T.Identifier)

    env_spec = EnvSpec(set_initial_env_by_name(
        Self.id.symbol, No(T.LexicalEnv)
    ))

    @lazy_field(public=True)
    def synth():
        return T.SynthNode.new(id=Self.id)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              strict_sound_envs=True)
print('Done')
