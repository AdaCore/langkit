"""
Test that nested grouped envs with non-null default metadata behave as
expected.
"""

from langkit.dsl import ASTNode, Field, Struct, T, UserField, env_metadata
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Entity, No, Self, Var, langkit_property

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    foo_node = UserField(T.FooNode)
    bar_node = UserField(T.FooNode)


class FooNode(ASTNode):
    @langkit_property(memoized=True)
    def env_with_md(foo_node=T.FooNode, bar_node=T.FooNode):
        md1 = Var(Metadata.new(
            foo_node=foo_node,
            bar_node=No(T.FooNode)
        ))
        md2 = Var(Metadata.new(
            foo_node=No(T.FooNode),
            bar_node=bar_node
        ))
        return Self.node_env.singleton.env_group(
            with_md=md1
        ).singleton.env_group(
            with_md=md2
        )

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def get_with_md(
            name=T.Symbol,
            foo_node=T.FooNode,
            bar_node=T.FooNode
    ):
        return Entity.env_with_md(foo_node, bar_node).get_first(name)

    @langkit_property(return_type=T.FooNode, public=True)
    def get_foo_metadata():
        return Entity.info.md.foo_node

    @langkit_property(return_type=T.FooNode, public=True)
    def get_bar_metadata():
        return Entity.info.md.bar_node


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


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
