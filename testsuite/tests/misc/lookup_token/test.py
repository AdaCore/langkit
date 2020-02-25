"""
Test that sloc-based token lookup works properly.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.lexer import (Ignore, Lexer, LexerToken, Literal, Pattern,
                           WithText, WithTrivia)

from utils import build_and_run


class Token(LexerToken):
    Example = WithText()
    Comment = WithTrivia()


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Ignore()),
    (Literal('example'),     Token.Example),
    (Pattern('#(.?)+'),      Token.Comment),
)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              lexer=foo_lexer, py_script='main.py', ada_main='main.adb')
print('Done')
