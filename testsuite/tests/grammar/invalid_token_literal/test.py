"""
Check that invalid token literals in the grammar are properly reported.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.parsers import Grammar, List

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(ExampleNode('example'), sep='\n')
)
emit_and_print_errors(grammar)
print('Done')
