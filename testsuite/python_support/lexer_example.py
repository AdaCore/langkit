from langkit.lexer import Eof, Lexer, LexerToken, Literal, NoText


class Token(LexerToken):
    Example = NoText()


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Eof(),              Token.Termination),
    (Literal("example"), Token.Example),
)
