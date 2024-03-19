from langkit.lexer import (Ignore, Lexer, LexerToken, Literal, Pattern,
                           WithSymbol, WithText, WithTrivia)


class Token(LexerToken):
    RshAssign = WithText()
    Is = WithText()
    Equals = WithText()
    Def = WithText()
    Lte = WithText()
    Raise = WithText()
    Mod = WithText()
    Yield = WithText()
    XorAssign = WithText()
    As = WithText()
    Lambda = WithText()
    Backtick = WithText()
    Try = WithText()
    Divide = WithText()
    Invert = WithText()
    Return = WithText()
    Assert = WithText()
    Xor = WithText()
    Break = WithText()
    Rbrack = WithText(end_ignore_layout=True)
    PowerAssign = WithText()
    Import = WithText()
    Exec = WithText()
    Comma = WithText()
    LPar = WithText(start_ignore_layout=True)
    Dot = WithText()
    Gte = WithText()
    FloordivAssign = WithText()
    Multiply = WithText()
    DivAssign = WithText()
    At = WithText()
    Assign = WithText()
    Floordiv = WithText()
    Notequal = WithText()
    MultAssign = WithText()
    ModAssign = WithText()
    Gt = WithText()
    Power = WithText()
    Amp = WithText()
    Not = WithText()
    Colon = WithText()
    Diamond = WithText()
    In = WithText()
    LCurl = WithText(start_ignore_layout=True)
    Class = WithText()
    OrAssign = WithText()
    Elif = WithText()
    And = WithText()
    Semicolon = WithText()
    AddAsign = WithText()
    Print = WithText()
    Lsh = WithText()
    Continue = WithText()
    While = WithText()
    Except = WithText()
    If = WithText()
    Else = WithText()
    Del = WithText()
    MinusAssign = WithText()
    Or = WithText()
    Minus = WithText()
    Lbrack = WithText(start_ignore_layout=True)
    AndAssign = WithText()
    RPar = WithText(end_ignore_layout=True)
    Global = WithText()
    For = WithText()
    From = WithText()
    Rsh = WithText()
    Finally = WithText()
    Pass = WithText()
    LshAssign = WithText()
    BinOr = WithText()
    Rcurl = WithText(end_ignore_layout=True)
    With = WithText()
    Plus = WithText()
    Lt = WithText()
    Number = WithText()
    String = WithText()
    Comment = WithTrivia()

    Identifier = WithSymbol()


python_lexer = Lexer(Token, track_indent=True, pre_rules=[
    (Pattern('\\\\\n[ \r\t]*'), Ignore())
])

python_lexer.add_patterns(
    ("STRING_DBQ", '"(\\\\"|[^\n"])*"'),
    ("STRING_SQ",  "'(\\\\'|[^\n'])*'"),
    ("MLSTRING_DBQ", '"""([^"]|("[^"])|(""[^"])|\n)*"""'),
    ("MLSTRING_SQ", "'''([^']|('[^'])|(''[^'])|\n)*'''"),
)

python_lexer.add_rules(
    (Pattern('(u|U)?(r|R)?'
             '({MLSTRING_SQ}|{MLSTRING_DBQ}'
             '|{STRING_SQ}|{STRING_DBQ})'), Token.String),
    (Pattern('[ \r\t]+'),   Ignore()),
    (Pattern("#(.?)+"),     Token.Comment),

    (Literal('>>='),         Token.RshAssign),
    (Literal('is'),          Token.Is),
    (Literal('=='),          Token.Equals),
    (Literal('def'),         Token.Def),
    (Literal('<='),          Token.Lte),
    (Literal('raise'),       Token.Raise),
    (Literal('%'),           Token.Mod),
    (Literal('yield'),       Token.Yield),
    (Literal('^='),          Token.XorAssign),
    (Literal('as'),          Token.As),
    (Literal('lambda'),      Token.Lambda),
    (Literal('`'),           Token.Backtick),
    (Literal('try'),         Token.Try),
    (Literal('/'),           Token.Divide),
    (Literal('~'),           Token.Invert),
    (Literal('return'),      Token.Return),
    (Literal('assert'),      Token.Assert),
    (Literal('^'),           Token.Xor),
    (Literal('break'),       Token.Break),
    (Literal(']'),           Token.Rbrack),
    (Literal('**='),         Token.PowerAssign),
    (Literal('import'),      Token.Import),
    (Literal('exec'),        Token.Exec),
    (Literal(','),           Token.Comma),
    (Literal('('),           Token.LPar),
    (Literal('.'),           Token.Dot),
    (Literal('>='),          Token.Gte),
    (Literal('//='),         Token.FloordivAssign),
    (Literal('*'),           Token.Multiply),
    (Literal('/='),          Token.DivAssign),
    (Literal('@'),           Token.At),
    (Literal('='),           Token.Assign),
    (Literal('//'),          Token.Floordiv),
    (Literal('!='),          Token.Notequal),
    (Literal('*='),          Token.MultAssign),
    (Literal('%='),          Token.ModAssign),
    (Literal('>'),           Token.Gt),
    (Literal('**'),          Token.Power),
    (Literal('&'),           Token.Amp),
    (Literal('not'),         Token.Not),
    (Literal(':'),           Token.Colon),
    (Literal('<>'),          Token.Diamond),
    (Literal('in'),          Token.In),
    (Literal('{'),           Token.LCurl),
    (Literal('class'),       Token.Class),
    (Literal('|='),          Token.OrAssign),
    (Literal('elif'),        Token.Elif),
    (Literal('and'),         Token.And),
    (Literal(';'),           Token.Semicolon),
    (Literal('+='),          Token.AddAsign),
    (Literal('print'),       Token.Print),
    (Literal('<<'),          Token.Lsh),
    (Literal('continue'),    Token.Continue),
    (Literal('while'),       Token.While),
    (Literal('except'),      Token.Except),
    (Literal('if'),          Token.If),
    (Literal('else'),        Token.Else),
    (Literal('del'),         Token.Del),
    (Literal('-='),          Token.MinusAssign),
    (Literal('or'),          Token.Or),
    (Literal('-'),           Token.Minus),
    (Literal('['),           Token.Lbrack),
    (Literal('&='),          Token.AndAssign),
    (Literal(')'),           Token.RPar),
    (Literal('global'),      Token.Global),
    (Literal('for'),         Token.For),
    (Literal('from'),        Token.From),
    (Literal('>>'),          Token.Rsh),
    (Literal('finally'),     Token.Finally),
    (Literal('pass'),        Token.Pass),
    (Literal('<<='),         Token.LshAssign),
    (Literal('|'),           Token.BinOr),
    (Literal('}'),           Token.Rcurl),
    (Literal('with'),        Token.With),
    (Literal('+'),           Token.Plus),
    (Literal('<'),           Token.Lt),
    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),
)
