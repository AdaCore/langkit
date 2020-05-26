from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Ref(FooNode):
    name = Field(type=Name)

    @langkit_property(public=True)
    def referenced():
        return Self.referenced_env.env_node.as_bare_entity

    @langkit_property(memoized=True)
    def referenced_env():
        return Self.node_env.get(Self.name.symbol).at(0).children_env


class Block(FooNode):
    name = Field(type=Name)
    content = Field(type=Ref.list)

    env_spec = EnvSpec(
        add_env(),
        add_to_env_kv(key=Self.name.symbol, val=Self,
                      dest_env=Self.node_env),
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
