"""
Test that a warning is emitted when the type of a parsing field is not as
specific as it could be.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleWrapper(FooNode):
    example = Field(type=FooNode)


class Example(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=ExampleWrapper(Example(Tok(Token.Example))),
)

emit_and_print_errors(foo_grammar)

print('Done')
