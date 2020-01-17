"""
Test that categories are properly considered during lexical env lookups.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import RefCategories, Self, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field(type=T.Name)
    cat1 = Field(type=T.Cat1)
    cat2 = Field(type=T.Cat2)
    example = Field(type=T.Example)

    env_spec = EnvSpec(add_env())


class Cat1(FooNode):
    decls = Field(type=T.Var.list)

    env_spec = EnvSpec(add_env())


class Cat2(FooNode):
    decls = Field(type=T.Var.list)

    env_spec = EnvSpec(add_env())


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(
        add_env(),
        reference([Self.parent.cast(Def).cat1.cast(T.FooNode)],
                  T.FooNode.children_env,
                  category='cat1'),
        reference([Self.parent.cast(Def).cat2.cast(T.FooNode)],
                  T.FooNode.children_env,
                  category='cat2')
    )

    @langkit_property(public=True)
    def lookup_all(name=T.Symbol):
        return Self.children_env.get(name)

    @langkit_property(public=True)
    def lookup_1(name=T.Symbol):
        return Self.children_env.get(name, categories=RefCategories(cat1=True))

    @langkit_property(public=True)
    def lookup_2(name=T.Symbol):
        return Self.children_env.get(name, categories=RefCategories(cat2=True))


class Name(FooNode):
    token_node = True


class Var(FooNode):
    name = Field(type=T.Name)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))


G = Grammar('main_rule')
G.add_rules(
    main_rule=Def('def', G.name, '{',
                  G.cat1,
                  G.cat2,
                  Example('example'),
                  '}'),
    cat1=Cat1(Token.Identifier('cat1'), '{', G.decls, '}'),
    cat2=Cat2(Token.Identifier('cat2'), '{', G.decls, '}'),
    decls=List(G.var, empty_valid=True),
    var=Var(G.name),
    name=Name(Token.Identifier),
)

build_and_run(G, 'main.py')
print('Done')
