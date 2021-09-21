"""
Check that named_env's or_current argument works as expected.
"""

from langkit.dsl import ASTNode, Field, LookupKind, T, has_abstract_list
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import If, Let, No, Self, langkit_property, named_env

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
    dest_scope = Field(type=T.Id)
    content = Field(type=T.Id.list)

    @langkit_property()
    def has_kw(kw=T.Symbol):
        """
        Return whether "content" contains the ``kw`` keyword.
        """
        return Self.content.map(lambda c: c.symbol).contains(kw)

    env_spec = EnvSpec(
        add_env(names=[Self.name.symbol]),
        add_to_env_kv(
            key=Self.name.symbol,
            val=Self,

            # Check correct behavior when the env argument is null or not (when
            # it's not, add to the parents' env), and when or_current=True is
            # passed or not (current = Self's env).
            dest_env=Let(
                lambda name=If(
                    Self.has_kw("null_name"),
                    No(T.Symbol),
                    Self.dest_scope.symbol,
                ):

                If(
                    Self.has_kw("or_current"),
                    named_env(name, or_current=True),
                    named_env(name),
                )
            )
        ),
    )


class ToplevelList(Example.list):
    env_spec = EnvSpec(add_env())


class Id(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script="main.py")
print('Done')
