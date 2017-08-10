from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, abstract
from langkit.parsers import Grammar, Row

from os import path

from utils import emit_and_print_errors

Diagnostics.set_lang_source_dir(path.abspath(__file__))


@abstract
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


class UnreferencedNode(FooNode):
    untyped_field = Field()


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Row('example') ^ ExampleNode
)
emit_and_print_errors(grammar)
print('Done')
