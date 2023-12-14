"""
Check that direct_env's or_current argument works as expected.
"""

from langkit.dsl import ASTNode, Field, LookupKind, T, has_abstract_list
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import If, Let, No, Self, direct_env, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def dump_env():
        return Self.children_env.get(
            symbol=No(T.Symbol),
            lookup=LookupKind.minimal,
        )


@has_abstract_list
class Example(FooNode):
    name = Field(type=T.Id)
    content = Field(type=T.Id.list)

    @langkit_property()
    def has_kw(kw=T.Symbol):
        """
        Return whether "content" contains the ``kw`` keyword.
        """
        return Self.content.map(lambda c: c.symbol).contains(kw)

    env_spec = EnvSpec(
        add_env(),
        add_to_env_kv(
            key=Self.name.symbol,
            value=Self,

            # Check correct behavior when the env argument is null or not (when
            # it's not, add to the parents' env), and when or_current=True is
            # passed or not (current = Self's env).
            dest_env=Let(
                lambda env=If(
                    Self.has_kw("null_env"),
                    No(T.LexicalEnv),
                    Self.parent.children_env,
                ):

                If(
                    Self.has_kw("or_current"),
                    direct_env(env, or_current=True),
                    direct_env(env),
                )
            )
        ),
    )


class ToplevelList(Example.list):
    env_spec = EnvSpec(add_env())


class Id(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script="main.py",
    types_from_lkt=True,
)
print('Done')
