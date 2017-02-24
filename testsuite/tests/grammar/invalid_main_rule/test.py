from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit.compiled_types import ASTNode, abstract, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, Row

from os import path

from utils import emit_and_print_errors

Diagnostics.set_lang_source_dir(path.abspath(__file__))


def lang_def():
    @abstract
    @root_grammar_class()
    class FooNode(ASTNode):
        pass

    class ExampleNode(FooNode):
        pass

    foo_grammar = Grammar('main_rulezz')
    foo_grammar.add_rules(
        main_rule=Row('example') ^ ExampleNode
    )

    return foo_grammar


emit_and_print_errors(lang_def)
print('Done')
