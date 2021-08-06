"""
Test that cross-unit lexical env associations have consistent sorting, so that
lexical env request behave the same way no matter the order in which lexical
envs were populated by cross units.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import (EnvSpec, add_env, add_to_env_kv,
                          set_initial_env_by_name)
from langkit.expressions import (AbstractKind, EmptyEnv, No, Self, String, Var,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Name(FooNode):

    @langkit_property(return_type=T.String)
    def scope_fqn():
        return Self.match(
            lambda dn=T.DottedName: dn.prefix.fqn,
            lambda _=T.Identifier: No(T.String),
        )

    @langkit_property(return_type=T.String)
    def fqn():
        return Self.match(
            lambda dn=T.DottedName:
                dn.prefix.fqn.concat(String(".").concat(dn.suffix.fqn)),
            lambda id=T.Identifier:
                id.text
        )

    @langkit_property(return_type=T.Symbol, kind=AbstractKind.abstract)
    def referenced_name():
        pass

    @langkit_property(return_type=T.LexicalEnv, kind=AbstractKind.abstract)
    def referenced_scope():
        pass


class Identifier(Name):
    token_node = True

    @langkit_property()
    def referenced_name():
        return Self.symbol

    @langkit_property(activate_tracing=True)
    def resolve_scope(from_env=T.LexicalEnv):
        env = Var(from_env.then(lambda e: e,
                                default_val=Self.referenced_parent_scope))
        return env.get(Self.symbol).at(0).children_env

    @langkit_property(activate_tracing=True)
    def referenced_parent_scope():
        return Self.unit.root.children_env

    @langkit_property(activate_tracing=True)
    def referenced_scope():
        return Self.resolve_scope(EmptyEnv)


class DottedName(Name):
    prefix = Field(type=Name)
    suffix = Field(type=Identifier)

    @langkit_property()
    def referenced_name():
        return Self.suffix.referenced_name

    @langkit_property()
    def referenced_parent_scope():
        return Self.prefix.referenced_scope

    @langkit_property()
    def referenced_scope():
        return Self.suffix.resolve_scope(Self.referenced_parent_scope)


class Scope(FooNode):
    name = Field(type=Name)
    content = Field()

    env_spec = EnvSpec(
        set_initial_env_by_name(
            Self.name.scope_fqn.then(lambda s: s.to_symbol, No(T.Symbol)),
            Self.parent.children_env,
        ),
        add_to_env_kv(key=Self.name.referenced_name, val=Self),
        add_env(names=[Self.name.fqn.to_symbol])
    )


class Ref(FooNode):
    name = Field(type=Name)

    @langkit_property(public=True)
    def resolve():
        return Self.name.referenced_scope.env_node


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
