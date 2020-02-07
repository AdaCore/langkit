from __future__ import absolute_import, division, print_function

from langkit.lexer import (
    Ignore, Lexer, LexerToken, Literal, Pattern, WithSymbol, WithText,
    WithTrivia
)


class Token(LexerToken):
    Identifier = WithSymbol()

    # Operators
    ExclMark = WithText()
    Colon = WithText()
    Semicolon = WithText()
    IntMark = WithText()
    Div = WithText()
    Comma = WithText()
    Times = WithText()
    Plus = WithText()
    Amp = WithText()
    Minus = WithText()
    Pipe = WithText()
    LeftArrow = WithText()
    FatRightArrow = WithText()
    Dot = WithText()
    Comb = WithText()
    At = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrace = WithText()
    RBrace = WithText()
    LBrack = WithText()
    RBrack = WithText()
    ListPlus = WithText()
    ListStar = WithText()
    Equal = WithText()
    GTE = WithText()
    LTE = WithText()
    GT = WithText()
    LT = WithText()
    Percent = WithText()

    # Keywords
    GrammarKw = WithText()
    OrKw = WithText()
    NotKw = WithText()
    ClassKw = WithText()
    StructKw = WithText()
    FunKw = WithText()
    PublicKw = WithText()
    PrivateKw = WithText()
    NullKw = WithText()
    IsaKw = WithText()
    ValKw = WithText()
    IfKw = WithText()
    ThenKw = WithText()
    ElifKw = WithText()
    ElseKw = WithText()
    AndKw = WithText()
    OrKw = WithText()
    BindKw = WithText()
    MatchKw = WithText()
    CaseKw = WithText()
    RaiseKw = WithText()
    TryKw = WithText()
    EnumKw = WithText()
    GenericKw = WithText()

    # Trivia
    Comment = WithTrivia()

    # Literals
    String = WithText()
    DocComment = WithText()
    Number = WithText()


lkt_lexer = Lexer(Token)

lkt_lexer.add_patterns(
    ("STRING_DBQ", r'\"(\\\"|[^\n\"])*\"'),
    ("STRING_SQ",  r"'(\\'|[^\n'])*'"),
)


lkt_lexer.add_rules(
    # Whitespace & EOF
    (Pattern(r"[ \t\r\n\f]+"), Ignore()),

    # Operators
    (Literal('!'),         Token.ExclMark),
    (Literal(';'),         Token.Semicolon),
    (Literal(':'),         Token.Colon),
    (Literal('?'),         Token.IntMark),
    (Literal('/'),         Token.Div),
    (Literal('*'),         Token.Times),
    (Literal('+'),         Token.Plus),
    (Literal('&'),         Token.Amp),
    (Literal('-'),         Token.Minus),
    (Literal('|'),         Token.Pipe),
    (Literal('<-'),        Token.LeftArrow),
    (Literal('.'),         Token.Dot),
    (Literal('('),         Token.LPar),
    (Literal(')'),         Token.RPar),
    (Literal('['),         Token.LBrack),
    (Literal(']'),         Token.RBrack),
    (Literal('{'),         Token.LBrace),
    (Literal('}'),         Token.RBrace),
    (Literal('|>'),        Token.Comb),
    (Literal(','),         Token.Comma),
    (Literal('@'),         Token.At),
    (Literal('list+'),     Token.ListPlus),
    (Literal('list*'),     Token.ListStar),
    (Literal('=>'),        Token.FatRightArrow),
    (Literal('='),         Token.Equal),
    (Literal('<='),        Token.LTE),
    (Literal('>='),        Token.GTE),
    (Literal('<'),         Token.LT),
    (Literal('>'),         Token.GT),
    (Literal('%'),         Token.Percent),

    # Keywords
    (Literal('grammar'),   Token.GrammarKw),
    (Literal('class'),     Token.ClassKw),
    (Literal('struct'),    Token.StructKw),
    (Literal('fun'),       Token.FunKw),
    (Literal('public'),    Token.PublicKw),
    (Literal('private'),   Token.PrivateKw),
    (Literal('null'),      Token.NullKw),
    (Literal('isa'),       Token.IsaKw),
    (Literal('val'),       Token.ValKw),
    (Literal('if'),        Token.IfKw),
    (Literal('elif'),      Token.ElifKw),
    (Literal('else'),      Token.ElseKw),
    (Literal('then'),      Token.ThenKw),
    (Literal('and'),       Token.AndKw),
    (Literal('or'),        Token.OrKw),
    (Literal('not'),       Token.NotKw),
    (Literal('bind'),      Token.BindKw),
    (Literal('match'),     Token.MatchKw),
    (Literal('case'),      Token.CaseKw),
    (Literal('raise'),     Token.RaiseKw),
    (Literal('try'),       Token.TryKw),
    (Literal('enum'),      Token.EnumKw),
    (Literal('generic'),   Token.GenericKw),

    # Identifiers
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*[!?]?'), Token.Identifier),

    # Numbers
    (Pattern('[0-9]+'),      Token.Number),

    # Strings
    (Pattern('{STRING_SQ}|{STRING_DBQ}'), Token.String),

    # Comments
    (Pattern(r"#(.?)+"),     Token.Comment),
    (Pattern(r"##(.?)+"),     Token.DocComment),
)
