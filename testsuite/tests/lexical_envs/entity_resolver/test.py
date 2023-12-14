"""
Test the handling of analysis units in the properties DSL.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def resolve_ref():
        return Self.match(
            lambda r=T.Ref: r.parent.parent.node_env.get(r.name).at(0),
            lambda _: No(T.entity),
        )


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
        key=Self.name.symbol, value=Self,
        resolver=FooNode.resolve_ref
    ))

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get(Self.name).at(0)


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
