from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import ASTNode, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.envs import EnvSpec
from langkit.expressions import Self
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    env_spec = EnvSpec(env_hook_arg=Self)


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Row('example') ^ BarNode,
    )
    return foo_grammar


emit_and_print_errors(lang_def)
print('Done')
