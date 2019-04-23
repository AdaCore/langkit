"""
Test that thanks to the PLE unit mechanism, the PLE pass can process PLE units
in an arbitrary order, so that first nodes in the top-level list can use
lexical environments created by list items that appear later.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import Annotations, ASTNode, Field, LookupKind, T, abstract
from langkit.envs import EnvSpec, add_to_env_kv, add_env, do, set_initial_env
from langkit.expressions import AbstractProperty, New, Self, langkit_property
from langkit.parsers import Grammar, List, Or, Pick

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Name(FooNode):
    resolve = AbstractProperty(T.FooNode.entity, public=True)
    suffix_symbol = AbstractProperty(T.Symbol, public=True)
    symbols = AbstractProperty(T.Symbol.array, public=True)

    @langkit_property(return_type=T.AnalysisUnit, external=True,
                      uses_entity_info=False, uses_envs=False)
    def referenced_unit_or_error(or_error=T.Bool):
        pass

    @langkit_property(public=True, return_type=T.AnalysisUnit)
    def referenced_unit():
        return Self.referenced_unit_or_error(False)


class Id(Name):
    token_node = True

    @langkit_property()
    def resolve():
        return Self.node_env.get_first(Self)

    @langkit_property()
    def suffix_symbol():
        return Self.symbol

    @langkit_property()
    def symbols():
        return Self.symbol._.singleton


class Prefix(Name):
    prefix = Field()
    suffix = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.prefix.resolve.children_env.get_first(
            Self.suffix.symbol,
            lookup=LookupKind.flat,
        )

    @langkit_property()
    def suffix_symbol():
        return Self.suffix.symbol

    @langkit_property()
    def symbols():
        return Self.prefix.symbols.concat(Self.suffix.symbols)


class Scope(FooNode):
    name = Field()
    deps = Field()
    defs = Field()

    annotations = Annotations(ple_unit_root=True)

    @langkit_property()
    def initial_env():
        return Self.name.match(
            lambda p=T.Prefix:
                p.prefix.referenced_unit.root.cast_or_raise(T.Scope.list)
                .filter(lambda scope: scope.name.symbols == p.prefix.symbols)
                .at_or_raise(0).children_env,
            lambda _:
                Self.children_env,
        )

    env_spec = EnvSpec(
        set_initial_env(Self.initial_env),
        add_to_env_kv(key=Self.name.suffix_symbol, val=Self),
        add_env(),
    )


class Dep(FooNode):
    name = Field()

    env_spec = EnvSpec(do(Self.name.referenced_unit))


class Var(FooNode):
    name = Field()
    value = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
    )


G = Grammar('main_rule')
G.add_rules(
    main_rule=List(G.scope),

    deps=List(Pick(Dep(Pick('+', G.name)), ';'), empty_valid=True),

    defs=List(Pick(Or(G.scope, G.var), ';'), empty_valid=True),

    scope=Scope(G.name, '{', G.deps, G.defs, '}'),
    var=Var(G.id, '=', G.name),

    id=Id(Token.Identifier),
    name=Or(Prefix(G.name, '.', G.id), G.id),
)
build_and_run(G, ada_main='main.adb')
print('Done')
