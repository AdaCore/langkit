"""
Test that using a named env as parent env works as expected when that parent
env is updated. In particular, we used to not invalidate cache entries of env
queries that needed to traverse the old parent, and in turn we would get
inconsistent results when doing the same queries after the parent had been
updated.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env_kv, set_initial_env
from langkit.expressions import (
    AbstractProperty, No, Self, String, direct_env, langkit_property, named_env
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Name(FooNode):
    resolve = AbstractProperty(T.FooNode.entity, public=True)
    suffix_symbol = AbstractProperty(T.Symbol, public=True)

    @langkit_property(return_type=T.String)
    def scope_fqn():
        return Self.match(
            lambda p=T.Prefix: p.prefix.fqn,
            lambda _=T.Id: No(T.String),
        )

    @langkit_property(return_type=T.String)
    def fqn():
        return Self.match(
            lambda p=T.Prefix:
                p.prefix.fqn.concat(String(".").concat(p.suffix.fqn)),
            lambda i=T.Id:
                i.text
        )


class Id(Name):
    token_node = True

    @langkit_property()
    def resolve():
        return Self.node_env.get_first(Self)

    @langkit_property()
    def suffix_symbol():
        return Self.symbol


class Prefix(Name):
    prefix = Field()
    suffix = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.prefix.resolve._.children_env.get_first(Self.suffix.symbol)

    @langkit_property()
    def suffix_symbol():
        return Self.suffix.symbol


class Scope(FooNode):
    name = Field()
    defs = Field()

    env_spec = EnvSpec(
        set_initial_env(
            Self.name.scope_fqn.then(
                lambda s: named_env(s.to_symbol),
                direct_env(Self.parent.children_env),
            ),
        ),
        add_to_env_kv(key=Self.name.suffix_symbol, value=Self),
        add_env(names=[Self.name.fqn.to_symbol]),
    )


class Var(FooNode):
    name = Field()
    value = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
    )


build_and_run(lkt_file="expected_concrete_syntax.lkt", gpr_mains=["main.adb"])
print("Done")
