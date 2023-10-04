"""
Test that nested grouped envs with non-null default metadata behave as
expected.
"""

from langkit.dsl import ASTNode, Field, MetadataField, Struct, T, env_metadata
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Entity, No, Self, Var, langkit_property

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    n1 = MetadataField(T.FooNode, use_in_eq=True)
    n2 = MetadataField(T.FooNode, use_in_eq=True)


class FooNode(ASTNode):
    @langkit_property(memoized=True)
    def env_with_md(n1=T.FooNode, n2=T.FooNode):
        md1 = Var(Metadata.new(n1=n1, n2=No(T.FooNode)))
        md2 = Var(Metadata.new(n1=No(T.FooNode), n2=n2))
        return (
            Self.node_env.singleton
            .env_group(with_md=md1).singleton
            .env_group(with_md=md2)
        )

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def get_with_md(name=T.Symbol, n1=T.FooNode, n2=T.FooNode):
        return Entity.env_with_md(n1, n2).get_first(name)

    @langkit_property(return_type=T.FooNode, public=True)
    def get_foo_metadata():
        return Entity.info.md.n1

    @langkit_property(return_type=T.FooNode, public=True)
    def get_bar_metadata():
        return Entity.info.md.n2


class Name(FooNode):
    token_node = True


class Decl(FooNode):
    name = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.name.symbol, value=Self
        ),
        add_env()
    )


class Ref(FooNode):
    name = Field()

    env_spec = EnvSpec(add_to_env_kv(
        key=Self.name.symbol, value=Self
    ))


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
