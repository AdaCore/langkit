from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, call_env_hook
from langkit.expressions import Self
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    env_spec = EnvSpec([call_env_hook(Self)])


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row('example') ^ BarNode,
    )
    return foo_grammar


emit_and_print_errors(lang_def)
print('Done')
