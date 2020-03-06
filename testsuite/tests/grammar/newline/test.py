"""
Test the newline and related parsers.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.lexer import (Ignore, Lexer, LexerToken, Literal, Pattern,
                           WithSymbol, WithText)

from utils import build_and_run, unparse_all_script


class Token(LexerToken):
    Example = WithText()
    Null = WithText()

    Comma = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrace = WithText()
    RBrace = WithText()
    Plus = WithText()

    Number = WithText()
    Identifier = WithSymbol()

foo_lexer = Lexer(Token, track_indent=True)
foo_lexer.add_rules(
    (Pattern(r'[ \r\t]+'), Ignore()),

    (Literal("example"), Token.Example),
    (Literal("null"),    Token.Null),

    (Literal(','), Token.Comma),
    (Literal('('), Token.LPar),
    (Literal(')'), Token.RPar),
    (Literal('{'), Token.LBrace),
    (Literal('}'), Token.RBrace),
    (Literal('+'), Token.Plus),

    (Pattern('[0-9]+'),                 Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),
)
L = foo_lexer


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    token_node = True


class NewLineNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()


class IndentNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()


class CompositeNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()
    lit_3 = Field()
    lit_4 = Field()

build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lexer=foo_lexer, unparse_script=unparse_all_script)
print('Done')
