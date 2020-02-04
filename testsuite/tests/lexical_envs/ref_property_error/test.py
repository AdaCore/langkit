"""
Test that there is no memory leak when a reference resolved aborts with a
property error.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.envs import EnvSpec, add_env, reference
from langkit.expressions import PropertyError, Self, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property()
    def resolve():
        return PropertyError(T.LexicalEnv)

    env_spec = EnvSpec(
        add_env(),
        reference([Self.cast(FooNode), Self.cast(FooNode)], T.Example.resolve),
    )


G = Grammar('main_rule')
G.add_rules(main_rule=Example('example'))

build_and_run(G, ada_main='main.adb')
print('Done')
