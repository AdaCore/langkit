from __future__ import absolute_import, division, print_function

from langkit.compiled_types import ASTNode, abstract, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, Row

from os import path

from utils import emit_and_print_errors

Diagnostics.set_lang_source_dir(path.abspath(__file__))


def lang_def():
    @abstract
    @root_grammar_class
    class FooNode(ASTNode):
        pass

    class ExampleNode(FooNode):
        pass

    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        sec_rule=Row('example'),
    )

    foo_grammar.add_rules(
        sec_rule=Row('example'),
        main_rule=Row(foo_grammar.sec_rule) ^ ExampleNode
    )

emit_and_print_errors(lang_def)
print('Done')
