from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env
from langkit.parsers import Grammar

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    env_spec = EnvSpec(add_env(), add_env())


grammar = Grammar('main_rule')
grammar.add_rules(main_rule=ExampleNode('example'))
emit_and_print_errors(grammar)
print('Done')
