from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.expressions import Property
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


def run(lit):
    class FooNode(ASTNode):
        b = Property(lit, public=True)

    grammar = Grammar('main_rule')
    grammar.add_rules(main_rule=Row('example') ^ FooNode)
    emit_and_print_errors(grammar)

print('Valid case')
run(12)
print('Valid case')
run('lol')
print('Invalid case')
run(12.90)
print('')
print('Done')
