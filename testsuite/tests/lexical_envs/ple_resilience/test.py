"""
Test that the populate lexical env pass is resilent to errors:

* It must resume traversal on siblings when getting a Property_Error from some
  node.

* It must initialize Self_Env fields for all nodes that are skipped because of
  a Property_Error.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env_kv, do
from langkit.expressions import (AbstractProperty, If, No, PropertyError, Self,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class HasError(FooNode):
    enum_node = True
    qualifier = True


@abstract
class Name(FooNode):
    resolve = AbstractProperty(T.FooNode.entity, public=True)


class Id(Name):
    token_node = True

    @langkit_property()
    def resolve():
        return Self.node_env.get_first(Self.symbol)


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
    error = Field()
    name = Field()
    defs = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env(),
        do(If(Self.error.as_bool,
              PropertyError(T.FooNode),
              No(T.FooNode))),
    )


class Var(DefNode):
    name = Field()
    value = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
    )


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
