"""
Check the absence of memory leak when a PropertyError is raised in the middle
of the lookup in a lexical env.

This test has a simple strategy: build envs (using a simple scope scheme) so
that a lexical env lookup starts collecting enough results in the Local_Results
vector during the lookup of parent environments to trigger a dynamic allocation
(Local_Results has a non-null small vector space), and so that the lookup of
referenced environments raises a PropertyError. The data in Local_Results used
to leak at that point.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import (EnvSpec, add_env, add_to_env_kv, handle_children,
                          reference)
from langkit.expressions import PropertyError, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Scope(FooNode):
    name = Field(type=T.Id)
    content = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env()
    )


class Id(FooNode):
    token_node = True

    @langkit_property()
    def resolve():
        return Self.node_env.get(Self.symbol)


class UseClause(FooNode):
    name = Field(type=T.Id)

    @langkit_property()
    def resolve():
        return Self.name.resolve().map(lambda n: n.children_env).env_group()

    env_spec = EnvSpec(
        handle_children(),
        reference(Self.cast(T.FooNode).singleton, T.UseClause.resolve)
    )


class ErrorUseClause(FooNode):

    @langkit_property()
    def resolve():
        return PropertyError(T.LexicalEnv, "unconditionally raised")

    env_spec = EnvSpec(
        handle_children(),
        reference(Self.cast(T.FooNode).singleton, T.ErrorUseClause.resolve)
    )


class Ref(FooNode):
    name = Field(type=T.Id)

    @langkit_property(public=True)
    def resolve():
        return Self.name.resolve


build_and_run(lkt_file="expected_concrete_syntax.lkt", gpr_mains=["main.adb"])
print("Done")
