"""
Test that sloc-based token lookup works properly.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.lexer import (Eof, Ignore, Lexer, LexerToken, Literal, Pattern,
                           WithText, WithTrivia)
from langkit.parsers import Grammar, List

from utils import build_and_run


class Token(LexerToken):
    Example = WithText()
    Comment = WithTrivia()


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Ignore()),
    (Eof(),                  Token.Termination),

    (Literal('example'),     Token.Example),
    (Pattern('#(.?)+'), Token.Comment),
)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
build_and_run(g, lexer=foo_lexer, py_script='main.py', ada_main='main.adb')
print('Done')
