"""
Test that covers syntactic problems uncovered during transition to concrete
syntax, that were not covered by the existing testsuite.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Var, langkit_property
from langkit.lexer import (Ignore, Lexer, LexerToken, Literal, Pattern,
                           WithText, WithTrivia)
from langkit.parsers import Grammar, List

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

    @langkit_property(public=True)
    def test_1():
        """
        Tests that block delimiter prevents a syntactic ambiguity.
        """
        a = Var(12)
        return [a]


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
build_and_run(g, lexer=foo_lexer, py_script='main.py')
print('Done')
