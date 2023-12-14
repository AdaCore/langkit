"""
Test the "get all" procedure of lexical envs, and in particular its
determinism.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(return_type=T.FooNode.entity.array, public=True)
    def env_get_all():
        return Self.children_env.get(symbol=No(T.Symbol))


class Id(FooNode):
    token_node = True


class Program(FooNode):
    name = Field(type=T.Id)
    program_list = Field(T.Program.list)
    env_spec = EnvSpec(
        add_to_env_kv(Self.name.symbol, Self),
        add_env()
    )


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
