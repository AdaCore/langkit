"""
Test the "get all" procedure of lexical envs, and in particular its determinism.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, has_abstract_list, synthetic
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Field,  No, Self, UserField, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(return_type=T.FooNode.entity.array,
                      public=True)
    def env_get_all():
        return Self.children_env.get(symbol=No(T.Symbol))

    @langkit_property(return_type=T.FooNode, memoized=True)
    def make_id(s=T.Symbol):
        return SynthId.new(sym=s)

    @langkit_property(return_type=T.FooNode)
    def unique_id(s=T.Symbol):
        return Self.unit.root.make_id(s)


@synthetic
class SynthId(FooNode):
    sym = UserField(type=T.Symbol, public=False)

    @langkit_property(return_type=T.String, public=True)
    def get_sym():
        return Self.sym.image


class Id(FooNode):
    token_node = True


@has_abstract_list
class Insert(FooNode):
    sym  = Field(type=T.Id)
    node = Field(type=T.Id)

    env_spec = EnvSpec(add_to_env_kv(
        Self.sym.symbol,
        Self.unique_id(Self.node.symbol)
    ))


class Program(Insert.list):
    env_spec=EnvSpec(
        add_env()
    )


G = Grammar('main_rule')
G.add_rules(
    main_rule=List(G.insert, list_cls=Program),
    insert=Insert('def', G.ident, G.ident),
    ident=Id(Token.Identifier),
)

build_and_run(G, 'main.py')
print('Done')
