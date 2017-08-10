"""
Test that invalid uses of analysis unit in the properties DSL are properly
detected and reported.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from utils import emit_and_print_errors


def run(name, prop):
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        result = Property(prop)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Row('example') ^ Example)
    emit_and_print_errors(grammar)
    print('')


run('Invalid field', Self.unit.foobar)
print('Done')
