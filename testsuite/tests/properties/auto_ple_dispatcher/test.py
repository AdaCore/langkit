"""
Check that Populate_Lexical_Env is automatically called in public property
dispatchers.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import AbstractProperty, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Name(FooNode):
    resolve = AbstractProperty(T.FooNode.entity, public=True)


class Id(Name):
    token_node = True

    @langkit_property()
    def resolve():
        return Self.node_env.get_first(Self)


class Prefix(Name):
    prefix = Field()
    suffix = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.prefix.resolve.children_env.get_first(Self.suffix.symbol)


@abstract
class DefNode(FooNode):
    pass


class Scope(DefNode):
    name = Field()
    defs = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env(),
    )


class Var(DefNode):
    name = Field()
    value = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
    )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
