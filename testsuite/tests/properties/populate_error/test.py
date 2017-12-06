from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, set_initial_env
from langkit.expressions import Self
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    env_spec = EnvSpec(
        set_initial_env(Self.parent.parent.children_env)
    )


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, 'script.py')
