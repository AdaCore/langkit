from __future__ import absolute_import, division, print_function

from langkit.compiled_types import (
    ASTNode, root_grammar_class, Field
)
from langkit.diagnostics import Diagnostics
from langkit.expressions import Env, Property, Self
from langkit.parsers import Grammar, Row, Tok

from os import path
from utils import emit_and_print_errors


def run(expr):
    """
    Emit and print the errors we get for the below grammar for the given
    "expr" property expression.
    """

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(expr))

    @root_grammar_class()
    class FooNode(ASTNode):
        pass

    class ExampleNode(FooNode):
        tok = Field()

        implicit_prop = Property(Self, has_implicit_env=True)

        prop = Property(expr, has_implicit_env=False, public=True)
        use_implicit_prop = Property(
            Self.node_env.eval_in_env(Self.implicit_prop),
            public=True
        )

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Row(Tok('example', keep=True)) ^ ExampleNode,
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run(Env.get(Self.tok))
run(Self.implicit_prop)
run(Self.node_env.eval_in_env(Env.get(Self.tok)))
run(Self.node_env.eval_in_env(Self.implicit_prop))
print('Done')
