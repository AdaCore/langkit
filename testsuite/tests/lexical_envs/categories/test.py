"""
Test that categories are properly considered during lexical env lookups.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import RefCategories, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field(type=T.Name)
    nocat = Field(type=T.NoCat)
    cat1 = Field(type=T.Cat1)
    cat2 = Field(type=T.Cat2)
    example = Field(type=T.Example)

    env_spec = EnvSpec(add_env())


class NoCat(FooNode):
    decls = Field(type=T.Var.list)

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
        reference([Self.parent.cast(Def).nocat.cast(T.FooNode)],
                  T.FooNode.children_env),
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

    @langkit_property()
    def lookup_by_cat(
        name=T.Symbol, cats=(T.RefCategories, RefCategories(default=False))
    ):
        return Self.children_env.get(name, categories=cats)

    @langkit_property(public=True)
    def lookup_none(name=T.Symbol):
        return Self.lookup_by_cat(name)

    @langkit_property(public=True)
    def lookup_1(name=T.Symbol):
        return Self.lookup_by_cat(name, cats=RefCategories(cat1=True))

    @langkit_property(public=True)
    def lookup_2(name=T.Symbol):
        return Self.lookup_by_cat(name, cats=RefCategories(cat2=True))


class Name(FooNode):
    token_node = True


class Var(FooNode):
    name = Field(type=T.Name)

    env_spec = EnvSpec(add_to_env_kv(Self.name.symbol, Self))


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
