"""
Test that invalid uses of analysis unit in the properties DSL are properly
detected and reported.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import ASTNode, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


def run(name, prop):
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
    print('== {} =='.format(name))

    @root_grammar_class
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        result = Property(prop)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(main_rule=Row('example') ^ Example)
        return foo_grammar
    emit_and_print_errors(lang_def)
    print('')


run('Invalid field', Self.unit.foobar)
print('Done')
