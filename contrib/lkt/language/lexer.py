from langkit.lexer import (
    Lexer, LexerToken, Literal, Pattern, TokenFamily, WithSymbol, WithText,
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
    TwoSidedArrow = WithText()
    LeftArrow = WithText()
    RightArrow = WithText()
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
    Equal = WithText()
    EQ = WithText()
    NE = WithText()
    GTE = WithText()
    LTE = WithText()
    GT = WithText()
    LT = WithText()
    Percent = WithText()

    # Keywords
    LexerKw = WithText()
    GrammarKw = WithText()
    OrKw = WithText()
    NotKw = WithText()
    ClassKw = WithText()
    StructKw = WithText()
    FunKw = WithText()
    PublicKw = WithText()
    PrivateKw = WithText()
    NullKw = WithText()
    IsKw = WithText()
    InKw = WithText()
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
    DiscardKw = WithText()
    ImportKw = WithText()
    ImplementsKw = WithText()
    TraitKw = WithText()
    DynVarKw = WithText()

    # Trivia
    Comment = WithTrivia(comment=True)
    Whitespace = WithTrivia()

    # Literals
    String = WithText()
    BlockStringLine = WithText()
    PString = WithText()
    Char = WithText()
    DocComment = WithText()
    Number = WithText()
    BigNumber = WithText()

    Alphanumericals = TokenFamily(
        Identifier,
        LexerKw,
        GrammarKw,
        OrKw,
        NotKw,
        ClassKw,
        StructKw,
        FunKw,
        PublicKw,
        PrivateKw,
        NullKw,
        IsKw,
        InKw,
        ValKw,
        IfKw,
        ThenKw,
        ElifKw,
        ElseKw,
        AndKw,
        OrKw,
        BindKw,
        MatchKw,
        CaseKw,
        RaiseKw,
        TryKw,
        EnumKw,
        GenericKw,
        DiscardKw,
        ImportKw,
        ImplementsKw,
        TraitKw,
        DynVarKw,
        PString,
        Number,
        BigNumber,
    )


lkt_lexer = Lexer(Token)

lkt_lexer.add_patterns(
    ("hex_digit", r'[0-9a-fA-F]'),
    ("hex_digits_2", r'{hex_digit}{hex_digit}'),
    ("hex_digits_4", r'{hex_digits_2}{hex_digits_2}'),
    ("hex_digits_8", r'{hex_digits_4}{hex_digits_4}'),
    ("string_lit", r'"(\\"|\\[^"]|[^\n"\\])*"'),
    ("char_lit", r"'(\\'|[^\n']*)'"),
)


lkt_lexer.add_rules(
    # Whitespace & EOF
    (Pattern(r"[ \t\r\n\f]+"), Token.Whitespace),

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
    (Literal('<->'),       Token.TwoSidedArrow),
    (Literal('<-'),        Token.LeftArrow),
    (Literal('->'),        Token.RightArrow),
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
    (Literal('=>'),        Token.FatRightArrow),
    (Literal('='),         Token.Equal),
    (Literal('=='),        Token.EQ),
    (Literal('!='),        Token.NE),
    (Literal('<='),        Token.LTE),
    (Literal('>='),        Token.GTE),
    (Literal('<'),         Token.LT),
    (Literal('>'),         Token.GT),
    (Literal('%'),         Token.Percent),

    # Keywords
    (Literal('lexer'),      Token.LexerKw),
    (Literal('grammar'),    Token.GrammarKw),
    (Literal('class'),      Token.ClassKw),
    (Literal('struct'),     Token.StructKw),
    (Literal('fun'),        Token.FunKw),
    (Literal('public'),     Token.PublicKw),
    (Literal('private'),    Token.PrivateKw),
    (Literal('null'),       Token.NullKw),
    (Literal('is'),         Token.IsKw),
    (Literal('in'),         Token.InKw),
    (Literal('val'),        Token.ValKw),
    (Literal('if'),         Token.IfKw),
    (Literal('elif'),       Token.ElifKw),
    (Literal('else'),       Token.ElseKw),
    (Literal('then'),       Token.ThenKw),
    (Literal('and'),        Token.AndKw),
    (Literal('or'),         Token.OrKw),
    (Literal('not'),        Token.NotKw),
    (Literal('bind'),       Token.BindKw),
    (Literal('match'),      Token.MatchKw),
    (Literal('case'),       Token.CaseKw),
    (Literal('raise'),      Token.RaiseKw),
    (Literal('try'),        Token.TryKw),
    (Literal('enum'),       Token.EnumKw),
    (Literal('generic'),    Token.GenericKw),
    (Literal('discard'),    Token.DiscardKw),
    (Literal('import'),     Token.ImportKw),
    (Literal('implements'), Token.ImplementsKw),
    (Literal('trait'),      Token.TraitKw),
    (Literal('dynvar'),     Token.DynVarKw),

    # Identifiers
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),

    # Numbers
    (Pattern('[0-9]+'),    Token.Number),
    (Pattern('[0-9]+b'),   Token.BigNumber),

    # Strings & chars
    (Pattern('{string_lit}'),         Token.String),
    (Pattern('[a-zA-Z]{string_lit}'), Token.PString),
    (Pattern('{char_lit}'),           Token.Char),
    (Pattern(r'\|"[^\r\n]*'),         Token.BlockStringLine),

    # Comments
    (Pattern(r"#(.?)+"),   Token.Comment),
)

lkt_lexer.add_spacing((Token.Alphanumericals, Token.Alphanumericals))
lkt_lexer.add_newline_after(
    Token.Comment,
    Token.BlockStringLine,
    Token.DocComment,
)
