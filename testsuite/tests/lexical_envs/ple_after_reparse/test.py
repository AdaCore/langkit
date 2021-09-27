"""
Test that 1) populate lexicale env is done asynchronously after a reparse and
that 2) semantic analysis still works after that.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env_kv, do, set_initial_env
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

    @langkit_property(return_type=T.AnalysisUnit, external=True,
                      uses_entity_info=False, uses_envs=False)
    def referenced_unit_or_error(or_error=T.Bool):
        pass

    @langkit_property(public=True, return_type=T.AnalysisUnit)
    def referenced_unit():
        return Self.referenced_unit_or_error(False)

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
        return Self.prefix.resolve.children_env.get_first(Self.suffix.symbol)

    @langkit_property()
    def suffix_symbol():
        return Self.suffix.symbol


class Scope(FooNode):
    name = Field()
    deps = Field()
    defs = Field()

    env_spec = EnvSpec(
        set_initial_env(
            Self.name.scope_fqn.then(
                lambda s: named_env(s.to_symbol),
                direct_env(Self.parent.children_env),
            ),
        ),
        add_to_env_kv(key=Self.name.suffix_symbol, val=Self),
        add_env(names=[Self.name.fqn.to_symbol]),
    )


class Dep(FooNode):
    name = Field()

    env_spec = EnvSpec(do(Self.name.referenced_unit))


class Var(FooNode):
    name = Field()
    value = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
