"""
Test that the populate lexical env pass is resilent to errors:

* It must resume traversal on siblings when getting a Property_Error from some
  node.

* It must initialize Self_Env fields for all nodes that are skipped because of
  a Property_Error.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, EnumNode, Field, T, abstract
from langkit.envs import EnvSpec, add_to_env, add_env, do
from langkit.expressions import (AbstractProperty, If, New, No, PropertyError,
                                 Self, langkit_property)
from langkit.parsers import Grammar, List, Opt, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class HasError(EnumNode):
    qualifier = True


@abstract
class Name(FooNode):
    resolve = AbstractProperty(T.FooNode.entity, public=True)


class Id(Name):
    tok = Field()

    @langkit_property()
    def resolve():
        return Self.node_env.get_first(Self.tok.symbol)


class Prefix(Name):
    prefix = Field()
    suffix = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.prefix.resolve.children_env.get_first(Self.suffix.symbol)


@abstract
class Def(FooNode):
    pass


class Scope(Def):
    error = Field()
    name = Field()
    defs = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.name.symbol, val=Self)),
        add_env(),
        do(If(Self.error.as_bool,
              PropertyError(T.FooNode),
              No(T.FooNode))),
    )


class Var(Def):
    name = Field()
    value = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.name.symbol, val=Self)),
    )


G = Grammar('main_rule')
G.add_rules(
    main_rule=G.defs,

    defs=List(G.def_rule, empty_valid=True),
    def_rule=Or(G.scope, G.var),

    scope=Scope(Opt('error').as_bool(HasError),
                Token.Identifier,
                '{', G.defs, '}'),
    var=Var(Token.Identifier, '=', G.name),

    name=Or(Prefix(G.name, '.', Token.Identifier),
            Id(Token.Identifier)),
)
build_and_run(G, 'main.py')
print('Done')
