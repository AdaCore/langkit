from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode
from langkit.expressions import Property
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


def make_lang_def(lit):
    def lang_def():
        class FooNode(ASTNode):
            b = Property(lit, public=True)

        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(main_rule=Row('example') ^ FooNode)
        return foo_grammar
    return lang_def

print('Valid case')
emit_and_print_errors(make_lang_def(12))
print('Valid case')
emit_and_print_errors(make_lang_def('lol'))
print('Invalid case')
emit_and_print_errors(make_lang_def(12.90))
print('')
print('Done')
