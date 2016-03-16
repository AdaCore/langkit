from langkit.compiled_types import root_grammar_class, StructMetaClass
from langkit.lexer import (
    Eof, Lexer, LexerToken, Literal, NoText, Pattern, WithText
)


class Token(LexerToken):
    Example = NoText()
    Null = NoText()

    Comma = NoText()

    Number = WithText()


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Eof(),              Token.Termination),

    (Literal("example"), Token.Example),
    (Literal("null"),    Token.Null),

    (Literal(','),       Token.Comma),

    (Pattern('[0-9]+'),  Token.Number),
)
