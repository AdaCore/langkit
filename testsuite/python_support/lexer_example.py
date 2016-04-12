from langkit.compiled_types import root_grammar_class, StructMetaClass
from langkit.lexer import (
    Eof, Ignore, Lexer, LexerToken, Literal, NoText, Pattern, WithSymbol,
    WithText
)


class Token(LexerToken):
    Example = NoText()
    Null = NoText()

    Comma = NoText()
    LPar = NoText()
    RPar = NoText()
    LBrace = NoText()
    RBrace = NoText()

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

    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_]+'),  Token.Identifier),
)
