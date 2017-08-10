from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, abstract
from langkit.parsers import Grammar, Row

from os import path

from utils import emit_and_print_errors

Diagnostics.set_lang_source_dir(path.abspath(__file__))


@abstract
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


grammar = Grammar('main_rule')
grammar.add_rules(
    sec_rule=Row('example'),
    main_rule=Row(grammar.sec_rules) ^ ExampleNode
)
emit_and_print_errors(grammar)
print('Done')
