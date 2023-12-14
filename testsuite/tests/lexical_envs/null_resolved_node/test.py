"""
Check that the lexical env lookups do not yield null entity when an entity
resolver turns a non-null entity into a null one.

Such null nodes used to be included, which showed up as crashes in "can_reach"
properties (Self was null).
"""

from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import No, Self, langkit_property, new_env_assoc

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def resolver():
        return No(T.FooNode.entity)


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(
        add_env(),
        add_to_env(
            new_env_assoc(
                key="foo",
                value=Self,
            ),
            resolver=T.FooNode.resolver,
        ),
    )

    @langkit_property(public=True)
    def get_all_foo():
        return Self.children_env.get("foo")


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
