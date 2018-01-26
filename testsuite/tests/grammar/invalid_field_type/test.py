"""
Test that invalid AST node parse fields are properly rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, TokenType, synthetic
from langkit.parsers import Grammar

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


@synthetic
class SynthExample(FooNode):
    f = Field(type=TokenType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))

emit_and_print_errors(foo_grammar)

print('Done')
