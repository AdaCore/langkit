"""
Test that add_to_env actions that try to insert foreign nodes (as mapping value
or metadata field) are properly rejected.
"""

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


class Scope(FooNode):
    name = Field(type=Identifier)
    content = Field()

    env_spec = EnvSpec(add_to_env_kv(key=Self.name.symbol, value=Self),
                       add_env())


class Decl(FooNode):
    id = Field(type=Identifier)

    env_spec = EnvSpec(add_to_env_kv(key=Self.id.symbol, value=Self))


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
