"""
Test that covers syntactic problems uncovered during transition to concrete
syntax, that were not covered by the existing testsuite.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import (
    ArrayLiteral, CharacterLiteral as Char, Let, String, Var, ignore,
    langkit_property
)
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
    def test_symlit_escape():
        """
        Test that sym literals with quotes in them are properly unparsed.
        """
        return "\"=\""

    @langkit_property(public=True)
    def test_dotexpr_lhs():
        """
        Test various valid dotexpr's LHS.
        """
        a = Var(ArrayLiteral([1]).find(lambda v: v == 1))
        b = Var(Let(lambda b=[1, 2]: b).find(lambda v: v == 1))
        c = Var(String("hello").find(lambda c: c == Char('h')))
        ignore(b)
        ignore(c)
        return a

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
