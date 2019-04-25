"""
Test that the ".p_node_env" property returns EmptyEnv even when the node's
self-env is also EmptyEnv. It used to return null, which is an erroneous
environment value in the DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, do
from langkit.expressions import Entity, PropertyError, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Decl(FooNode):
    names = Field()
    env_spec = EnvSpec(do(PropertyError(T.FooNode)), add_env())

    @langkit_property(public=True)
    def lookup(n=T.Name.entity):
        return Entity.node_env.get_first(n.symbol)


class Name(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.decl),
    decl=Decl('def', List(g.name)),
    name=Name(Token.Identifier),
)
build_and_run(g, 'main.py')
print('Done')
