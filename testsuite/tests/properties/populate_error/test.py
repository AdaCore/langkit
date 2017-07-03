from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, set_initial_env
from langkit.expressions import Self
from langkit.parsers import Grammar, Row

from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Example(FooNode):
    env_spec = EnvSpec(
        set_initial_env(Self.parent.parent.children_env)
    )


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Row('example') ^ Example,
)
build_and_run(foo_grammar, 'script.py')
