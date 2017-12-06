from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, abstract
from langkit.parsers import Grammar

from utils import emit_and_print_errors


@abstract
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


grammar = Grammar('main_rulezz')
grammar.add_rules(
    main_rule=ExampleNode('example')
)
emit_and_print_errors(grammar)
print('Done')
