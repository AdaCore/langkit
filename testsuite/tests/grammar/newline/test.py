"""
Test the newline and related parsers.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, root_grammar_class
from langkit.lexer import (
    Eof, Ignore, Lexer, LexerToken, Literal, Pattern, WithSymbol, WithText
)
from langkit.parsers import Grammar, Row, Tok, Or

from utils import build_and_run


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
    (Eof(),                  Token.Termination),

    (Literal("example"),     Token.Example),
    (Literal("null"),        Token.Null),

    (Literal(','),           Token.Comma),
    (Literal('('),           Token.LPar),
    (Literal(')'),           Token.RPar),
    (Literal('{'),           Token.LBrace),
    (Literal('}'),           Token.RBrace),
    (Literal('+'),           Token.Plus),

    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),
)
L = foo_lexer

Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class Literal(FooNode):
    tok = Field()


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

foo_grammar = Grammar('main_rule')
A = foo_grammar

foo_grammar.add_rules(
    lit=Row(Tok(Token.Number, keep=True)) ^ Literal,
    nl=NewLineNode(A.lit, L.Newline(), A.lit),
    ind=IndentNode(A.lit, L.Newline(), L.Indent(), A.lit, L.Dedent()),
    comp=CompositeNode(
        A.lit, L.Newline(),
        A.lit, L.Newline(), L.Indent(),
        A.lit, L.Newline(), L.Dedent(),
        A.lit
    ),
    main_rule=Or(A.comp, A.ind, A.nl)
)
build_and_run(foo_grammar, 'main.py', lexer=foo_lexer)
print('Done')
