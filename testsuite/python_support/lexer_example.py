from langkit.lexer import (
    Eof, Ignore, Lexer, LexerToken, Literal, Pattern, WithSymbol, WithText
)


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


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Ignore()),
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
    (Pattern('[a-zA-Z_]+'),  Token.Identifier),
)
