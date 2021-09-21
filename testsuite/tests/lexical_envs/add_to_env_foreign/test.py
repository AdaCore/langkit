"""
Test that add_to_env actions that try to insert foreign nodes (as mapping value
or metadata field) are properly rejected.
"""

from langkit.dsl import (ASTNode, Field, Struct, T, UserField, abstract,
                         env_metadata, synthetic)
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import (AbstractKind, If, New, Self, direct_env,
                                 langkit_property)

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    node = UserField(T.FooNode)


class FooNode(ASTNode):
    pass


class Scope(FooNode):
    name = Field(type=T.SimpleId)
    content = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
        add_env()
    )


@abstract
class Id(FooNode):
    @langkit_property(return_type=T.SimpleId, kind=AbstractKind.abstract)
    def simple_name():
        pass

    @langkit_property(return_type=T.Scope, kind=AbstractKind.abstract)
    def resolve(base_env=T.LexicalEnv):
        pass


class SimpleId(Id):
    token_node = True

    @langkit_property()
    def simple_name():
        return Self

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return base_env.get_first(Self.symbol).node.cast(T.Scope)


class ScopedId(Id):
    scope = Field(type=T.Id)
    name = Field(type=T.SimpleId)

    @langkit_property()
    def simple_name():
        return Self.name

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return (Self.scope.resolve(base_env)
                .children_env.get_first(Self.name.symbol).node
                .cast(T.Scope))


class Synth(FooNode):
    enum_node = True
    qualifier = True


@synthetic
class DummySyntheticNode(FooNode):
    pass


class ForeignDecl(FooNode):
    create_synthetic = Field(type=T.Synth)
    id = Field(type=T.Id)

    @langkit_property(memoized=True)
    def node_for_env():
        return If(Self.create_synthetic.as_bool,
                  New(T.DummySyntheticNode),
                  Self)

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.id.simple_name.symbol, val=Self.node_for_env,
            dest_env=direct_env(Self.id.match(
                lambda simple=T.SimpleId:
                    simple.node_env,
                lambda scoped=T.ScopedId:
                    scoped.resolve(Self.node_env).children_env,
            )),
        )
    )


class SelfDecl(FooNode):
    id = Field(type=T.Id)
    md_node = Field(type=T.Id)

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.id.simple_name.symbol,
            val=Self.id.resolve(Self.node_env),
            metadata=New(
                T.Metadata,
                node=Self.md_node.then(lambda n: n.resolve(Self.node_env))
            )
        )
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
