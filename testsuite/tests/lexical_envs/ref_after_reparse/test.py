"""
Test that the referenced envs link that cross analysis units stay valid after
reparsing.
"""

from langkit.dsl import ASTNode, Field, LexicalEnv
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import DynamicVariable, Self, langkit_property

from utils import build_and_run


Env = DynamicVariable('env', LexicalEnv)


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True

    @langkit_property(dynamic_vars=[Env])
    def ambiant_entity():
        return Env.get(Self.symbol).at(0)

    @langkit_property()
    def designated_env():
        return Self.unit.root.node_env.get(Self.symbol).at(0).children_env

    @langkit_property(public=True)
    def entity():
        return Env.bind(Self.node_env, Self.ambiant_entity)


class Block(FooNode):
    name = Field()
    decls = Field()
    usings = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env()
    )


class Decl(FooNode):
    name = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self)
    )


class Using(FooNode):
    name = Field()
    env_spec = EnvSpec(
        reference(Self.name.cast(FooNode)._.singleton,
                  through=Name.designated_env)
    )


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True)
    def entity():
        return Self.as_entity.name.entity


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print('Done')
