"""
Test that the referenced envs link that cross analysis units stay valid after
reparsing.
"""

from langkit.dsl import ASTNode, Field, LexicalEnv, abstract
from langkit.envs import EnvSpec, RefKind, add_env, add_to_env_kv, reference
from langkit.expressions import DynamicVariable, Self, langkit_property

from utils import build_and_run, unparse_all_script


Env = DynamicVariable("env", LexicalEnv)


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    pass


class Name(FooNode):
    token_node = True

    @langkit_property()
    def designated_env():
        return (
            Self.node_env.get(Self.symbol, from_node=Self).at(0).children_env
        )

    @langkit_property()
    def get_ref():
        return Self.node_env.get(Self.symbol, from_node=Self).at(0)


class Package(Decl):
    name = Field()
    decls = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env()
    )


class Ref(Decl):
    name = Field()

    @langkit_property(public=True)
    def entity():
        return Self.as_entity.name.get_ref


class Var(Decl):
    name = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self)
    )


class UseClause(Decl):
    name = Field()
    env_spec = EnvSpec(
        reference(Self.name.cast(FooNode)._.singleton,
                  through=Name.designated_env, kind=RefKind.transitive)
    )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    unparse_script=unparse_all_script,
    types_from_lkt=True,
)
print("Done")
