from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.parsers import Grammar

from utils import emit_and_print_errors


@abstract
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


class UnreferencedNode(FooNode):
    untyped_field = Field()


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=ExampleNode('example')
)
emit_and_print_errors(grammar)
print('Done')
