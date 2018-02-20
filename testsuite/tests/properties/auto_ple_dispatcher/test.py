"""
Check that Populate_Lexical_Env is automatically called in public property
dispatchers.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_to_env, add_env
from langkit.expressions import AbstractProperty, New, Self, langkit_property
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Name(FooNode):
    resolve = AbstractProperty(T.FooNode.entity, public=True)


class Id(Name):
    token_node = True

    @langkit_property()
    def resolve():
        return Self.node_env.get_first(Self)


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
    name = Field()
    defs = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.name.symbol, val=Self)),
        add_env(),
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

    scope=Scope(Id(Token.Identifier),
                '{', G.defs, '}'),
    var=Var(Id(Token.Identifier), '=', G.name),

    name=Or(Prefix(G.name, '.', Id(Token.Identifier)),
            Id(Token.Identifier)),
)
build_and_run(G, ada_main='main.adb')
print('Done')
