from __future__ import absolute_import, division, print_function

from langkit.lexer import (Lexer, LexerToken, Literal, Pattern, TokenFamily,
                           WithSymbol, WithText, WithTrivia)


class Token(LexerToken):
    Def = WithText()
    Var = WithText()
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
    Minus = WithText()
    LessThan = WithText()

    Number = WithText()
    Identifier = WithSymbol()
    String = WithText()

    Comment = WithTrivia()
    Whitespace = WithTrivia()

    Alphanumericals = TokenFamily(Def, Error, Example, Null, Number,
                                  Identifier)
    Punctuation = TokenFamily(Comma, Dot, Semicolon, LPar, RPar, LBrace,
                              RBrace, Equal, Plus)
    Comments = TokenFamily(Comment)


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Token.Whitespace),

    (Literal('def'),         Token.Def),
    (Literal('var'),         Token.Var),
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
    (Literal('-'),           Token.Minus),
    (Literal('<'),           Token.LessThan),

    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),
    (Pattern(r'\"[^\"]*\"'), Token.String),

    (Pattern('#(.?)+'), Token.Comment),
)
foo_lexer.add_spacing((Token.Alphanumericals, Token.Alphanumericals))
foo_lexer.add_newline_after(Token.Comment)
