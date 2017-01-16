import os.path

from langkit.compiled_types import ASTNode, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.envs import EnvSpec
from langkit.expressions import Self
from langkit.parsers import Grammar, Row

from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Example(FooNode):
    env_spec = EnvSpec(initial_env=Self.parent.parent.children_env)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Row('example') ^ Example,
)
build_and_run(foo_grammar, 'script.py')
