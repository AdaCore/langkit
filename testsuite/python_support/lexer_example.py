from __future__ import absolute_import, division, print_function

from langkit.lexer import (
    Eof, Ignore, Lexer, LexerToken, Literal, Pattern, TokenFamily, WithSymbol,
    WithText, WithTrivia
)


class Token(LexerToken):
    Def = WithText()
    Error = WithText()
    Example = WithText()
    Null = WithText()

    Comma = WithText()
    Dot = WithText()
    Semicolon = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrace = WithText()
    RBrace = WithText()
    Equal = WithText()
    Plus = WithText()

    Number = WithText()
    Identifier = WithSymbol()

    Comment = WithTrivia()

    Alphanumericals = TokenFamily(Def, Error, Example, Null, Number,
                                  Identifier)
    Punctuation = TokenFamily(Comma, Dot, Semicolon, LPar, RPar, LBrace,
                              RBrace, Equal, Plus)
    Comments = TokenFamily(Comment)


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Ignore()),
    (Eof(),                  Token.Termination),

    (Literal('def'),         Token.Def),
    (Literal('error'),       Token.Error),
    (Literal('example'),     Token.Example),
    (Literal('null'),        Token.Null),

    (Literal(','),           Token.Comma),
    (Literal('.'),           Token.Dot),
    (Literal(';'),           Token.Semicolon),
    (Literal('('),           Token.LPar),
    (Literal(')'),           Token.RPar),
    (Literal('{'),           Token.LBrace),
    (Literal('}'),           Token.RBrace),
    (Literal('='),           Token.Equal),
    (Literal('+'),           Token.Plus),

    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),

    (Pattern('#(.?)+'), Token.Comment),
)
