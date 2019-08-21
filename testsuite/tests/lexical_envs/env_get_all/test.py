"""
Test add_to_env with multiple env_assoc having different destination
environments.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import (ASTNode, T, has_abstract_list)
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import No, Self, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(return_type=T.FooNode.entity.array,
                      public=True)
    def env_get_all():
        return Self.children_env.get(symbol=No(T.Symbol))


@has_abstract_list
class Id(FooNode):
    token_node = True
    env_spec = EnvSpec(add_to_env_kv(Self.symbol, Self))


class Program(Id.list):
    env_spec=EnvSpec(
        add_env()
    )


G = Grammar('main_rule')
G.add_rules(
    main_rule=List(G.elem, list_cls=Program),
    elem=Id(Token.Identifier),
)

build_and_run(G, 'main.py')
print('Done')
