"""
Check that providing a custom "can_reach" has expected effects on env lookups.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import Self, ignore, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def can_reach(origin=T.FooNode):
        ignore(origin)
        # "foo" defs can never be reached: "foo" Def nodes should never appear
        # in the result of any lexical env lookups.
        return Self.match(
            lambda d=T.Def: d.name.symbol != "foo",
            lambda _: True,
        )

    @langkit_property(public=True)
    def lookup(n=T.Symbol):
        return Self.node_env.get(n, from_node=Self)


class Def(FooNode):
    name = Field(type=T.Name)
    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))


class Name(FooNode):
    token_node = True


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py")
print("Done")
