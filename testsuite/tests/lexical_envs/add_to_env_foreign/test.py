"""
Test that add_to_env actions that try to insert foreign nodes (as mapping value
or metadata field) are properly rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import (ASTNode, Field, Struct, T, UserField, abstract,
                         env_metadata)
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import AbstractKind, New, Self, langkit_property
from langkit.parsers import Grammar, List, Opt, Or

from lexer_example import Token
from utils import build_and_run


@env_metadata
class Metadata(Struct):
    node = UserField(T.FooNode)


class FooNode(ASTNode):
    pass


class Scope(FooNode):
    name = Field(type=T.SimpleId)
    content = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
        add_env()
    )


@abstract
class Id(FooNode):
    @langkit_property(return_type=T.SimpleId, kind=AbstractKind.abstract)
    def simple_name():
        pass

    @langkit_property(return_type=T.Scope, kind=AbstractKind.abstract)
    def resolve(base_env=T.LexicalEnv):
        pass


class SimpleId(Id):
    token_node = True

    @langkit_property()
    def simple_name():
        return Self

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return base_env.get_first(Self.symbol).node.cast(T.Scope)


class ScopedId(Id):
    scope = Field(type=T.Id)
    name = Field(type=T.SimpleId)

    @langkit_property()
    def simple_name():
        return Self.name

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return (Self.scope.resolve(base_env)
                .children_env.get_first(Self.name.symbol).node
                .cast(T.Scope))


class ForeignDecl(FooNode):
    id = Field(type=T.Id)

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.id.simple_name.symbol, val=Self,
            dest_env=Self.id.match(
                lambda simple=T.SimpleId:
                    simple.node_env,
                lambda scoped=T.ScopedId:
                    scoped.resolve(Self.node_env).children_env,
            )
        )
    )


class SelfDecl(FooNode):
    id = Field(type=T.Id)
    md_node = Field(type=T.Id)

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.id.simple_name.symbol,
            val=Self.id.resolve(Self.node_env),
            metadata=New(
                T.Metadata,
                node=Self.md_node.then(lambda n: n.resolve(Self.node_env))
            )
        )
    )


G = Grammar('main_rule')
G.add_rules(
    main_rule=List(Or(G.scope, G.self_decl, G.foreign_decl)),
    scope=Scope(G.simple_identifier,
                '{', List(G.scope, empty_valid=True), '}'),

    identifier=Or(ScopedId(G.identifier, '.', G.simple_identifier),
                  G.simple_identifier),
    simple_identifier=SimpleId(Token.Identifier),

    foreign_decl=ForeignDecl(G.identifier),
    self_decl=SelfDecl('+', G.identifier, Opt('(', G.identifier, ')')),
)
build_and_run(G, 'main.py')
print('Done')
