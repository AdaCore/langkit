"""
Test that the unparsing machinery rejects lexers with Ignore actions.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.lexer import Ignore, Lexer, LexerToken, Pattern, WithText
from langkit.parsers import Grammar

from utils import emit_and_print_errors


class BaseToken(LexerToken):
    Example = WithText()
    Whitespace = Ignore()


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


foo_lexer = Lexer(BaseToken)
foo_lexer.add_rules((Pattern('[ \t]+'),  BaseToken.Whitespace),
                    (Pattern('example'), BaseToken.Example))

g = Grammar('main_rule')
g.add_rules(main_rule=Example(BaseToken.Example))
emit_and_print_errors(g, foo_lexer, generate_unparser=True)

print('Done')
