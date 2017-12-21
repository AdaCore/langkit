from __future__ import absolute_import, division, print_function

from langkit.lexer import (
    Eof, Ignore, Lexer, LexerToken, Literal, Pattern, WithSymbol, WithText
)


class Token(LexerToken):
    Example = WithText()
    Null = WithText()
    Def = WithText()

    Comma = WithText()
    Dot = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrace = WithText()
    RBrace = WithText()
    Equal = WithText()
    Plus = WithText()

    Number = WithText()
    Identifier = WithSymbol()


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Ignore()),
    (Eof(),                  Token.Termination),

    (Literal('example'),     Token.Example),
    (Literal('null'),        Token.Null),
    (Literal('def'),         Token.Def),

    (Literal(','),           Token.Comma),
    (Literal('.'),           Token.Dot),
    (Literal('('),           Token.LPar),
    (Literal(')'),           Token.RPar),
    (Literal('{'),           Token.LBrace),
    (Literal('}'),           Token.RBrace),
    (Literal('='),           Token.Equal),
    (Literal('+'),           Token.Plus),

    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),
)
