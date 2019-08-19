"""
Test that node_env works correctly for a subclass of a concrete node with an
add_env directive.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, LookupKind as LK, T, abstract
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


@abstract
class FooNode(ASTNode):
    pass


class RootNode(FooNode):
    env_spec = EnvSpec(add_env())
    decls = Field()


@abstract
class BaseDecl(FooNode):

    @langkit_property(public=True)
    def lookup(n=T.Symbol):
        return Self.env_lookup(Self.node_env, n)

    @langkit_property()
    def env_lookup(env=T.LexicalEnv, n=T.Symbol):
        return env.get_first(n, lookup=LK.flat)


class Decl(BaseDecl):
    name = Field()
    env_spec = EnvSpec(
        add_to_env_kv(Self.name.symbol, Self),
        add_env()
    )


class SubDecl(Decl):
    pass


class OtherDecl(BaseDecl):
    name = Field()


class Name(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(
    main_rule=RootNode(List(g.decl | g.subdecl | g.other_decl, sep=";")),
    decl=Decl('def', g.name),
    other_decl=OtherDecl('def', 'var', g.name),
    subdecl=SubDecl('var', g.name),
    name=Name(Token.Identifier),
)
build_and_run(g, 'main.py')
print('Done')
