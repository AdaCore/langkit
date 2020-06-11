from collections import defaultdict
from itertools import count
import re

from langkit.compile_context import get_context
from langkit.diagnostics import (Context, check_source_language,
                                 extract_library_location)
from langkit.lexer.regexp import DFACodeGenHolder, NFAState, RegexpCollection
from langkit.names import Name


# All "signature" properties in classes below are used to identify the whole
# lexer specification, and thus implement a cache. If the signature does not
# change from one run to another, we can avoid computing the DFA and generating
# the state machine sources, which can be costly.


class Matcher:
    """
    Base class for a matcher. A matcher specificies in which case a given
    input will trigger a match.
    """

    def __init__(self, location=None):
        self.location = location or extract_library_location()

    @property
    def match_length(self):
        """
        Return the number of characters this pattern will accept, or raise
        ValueError if it is variable.
        :rtype: int
        """
        raise NotImplementedError()

    @property
    def regexp(self):
        """
        Return a regular expression (syntax for langkit.lexer.regexp) to
        implement this matcher.

        :rtype: str
        """
        raise NotImplementedError()

    @property
    def signature(self):
        raise NotImplementedError()


class Pattern(Matcher):
    r"""
    Regular-expression based matcher.

    The following subset of regular expressions is supported:

    * ``A|B``: match either the ``A`` pattern or the ``B`` one.

    * ``(A)``: match the ``A`` pattern. Useful to group patterns and change
      associativity.

    * ``[XYZ]``: match either the ``X`` character, the ``Y`` one or the ``Z``
      one.

    * ``[^XYZ]``: match any character but the ``X`` character, the ``Y`` one or
      the ``Z`` one.

    * ``[A-Z]``: match any character between ``A`` and ``Z``.

    * ``{NAME}``: reference to the pattern called ``NAME``. See
      ``Lexer.add_patterns`` for how to add named patterns.

    * ``A*``: match the ``A`` pattern zero, one or multiple times.

    * ``A+``: match the ``A`` pattern one or multiple times.

    * ``A?``: match the ``A`` pattern zero or one time.

    * ``.``: match any character except the newline one. Equivalent to
      ``[^\n]``.

    * ``\p{CATEGORY}``: match any character in the given Unicode general
      category.

    * ``\P{CATEGORY}``: match any character *not* in the given Unicode general
      category.

    Note that due to the intended use of these regular expressions, several
    traditional constructs are not supported:

    * ``^`` and ``$``, to match the very beginning of the input and its end.
    """

    def __init__(self, pattern, location=None):
        super().__init__(location)
        self.pattern = pattern

    @property
    def match_length(self):
        for c in self.pattern:
            check_source_language(
                re.escape(c) == c or c in ('.', '\''),
                'Cannot compute the maximum number of characters this pattern'
                ' will accept: {}'.format(repr(self.pattern))
            )
        return len(self.pattern)

    @property
    def regexp(self):
        return self.pattern

    @property
    def signature(self):
        return ('Pattern', self.pattern)


class Action:
    """
    Base class for an action. An action specificies what to do with a given
    match.
    """

    @property
    def is_case_action(self):
        return isinstance(self, Case.CaseAction)

    @property
    def is_ignore(self):
        return isinstance(self, Ignore)


class TokenAction(Action):
    """
    Abstract Base class for an action that sends a token. Subclasses of
    TokenAction can *only* be used as the instantiation of a token kind, in the
    declaration of a LexerToken subclass, as in::

        class MyToken(LexerToken):
            Identifier = WithSymbol()
            Keyword = WithText()
    """
    # This counter is used to preserve the order of TokenAction instantiations,
    # which allows us to get the declaration order of token enum kinds.
    _counter = iter(count(0))

    is_trivia = False

    def __init__(self, start_ignore_layout=False, end_ignore_layout=False):
        """
        Create a new token action. This is meant to be called on subclasses of
        TokenAction.

        :param bool start_ignore_layout: If True, the token associated with
            this token action will trigger the start of layout ignore, which
            means that indent, dedent, and newline tokens will not be emitted
            by the lexer.

        :param bool end_ignore_layout: If True, the token associated with this
            token action will trigger the end of layout ignorance.

        Note that layout ignore works in a nested fashion: If the lexer reads 3
        tokens that starts layout ignore, it will need to read 3 tokens that
        ends it so that it is taken into account again. The lexer won't handle
        proper pairing: This is up to the parser's implementer.
        """
        self._index = next(TokenAction._counter)

        self.name = None
        """
        Name user associated to this token.

        :type: names.Name
        """

        self.lexer = None
        self.start_ignore_layout = start_ignore_layout
        self.end_ignore_layout = end_ignore_layout

        self.matcher = None
        """
        If this action is associated to a Literal matcher, this will be set to
        it.
        :type: Matcher|None
        """

    @property
    def signature(self):
        return (type(self).__name__,
                self.name.camel,
                self.start_ignore_layout,
                self.end_ignore_layout)

    @property
    def value(self):
        return self._index

    def __call__(self, *args, **kwargs):
        """
        Shortcut to create token parsers in the grammar.
        """
        from langkit.parsers import _Token
        return _Token(self, *args, **kwargs)

    @property
    def dsl_name(self):
        """
        Name for this token as it appears in the DSL. To be used in
        diagnostics.

        :rtype: str
        """
        return self.name.camel

    @property
    def base_name(self):
        return self.name

    @property
    def ada_name(self):
        pname = get_context().lang_name + self.base_name
        return pname.camel_with_underscores

    @property
    def c_name(self):
        prefixed_name = get_context().lang_name + self.base_name
        return prefixed_name.upper

    def __repr__(self):
        return '<{} {}>'.format(type(self).__name__,
                                self.name.camel if self.name else '???')


class WithText(TokenAction):
    """
    TokenAction. The associated token kind will have the lexed text associated
    to it. A new string will be allocated by the parser each time. Suited for
    literals (numbers, strings, etc..)::

        class MyToken(LexerToken):
            # String tokens will keep the associated text when lexed
            StringLiteral = WithText()
    """
    pass


class WithTrivia(WithText):
    """
    TokenAction. The associated token kind will have the lexed text associated
    to it. A new string will be allocated by the parser each time. Suited for
    literals (numbers, strings, etc..)::

        class MyToken(LexerToken):
            # String tokens will keep the associated text when lexed
            StringLiteral = WithText()
    """
    is_trivia = True


class WithSymbol(TokenAction):
    """
    TokenAction. When the associated token kind will be lexed, a token will be
    created with the text corresponding to the match, but as an internalized
    symbol, so that if you have two tokens with the same text, the text will be
    shared amongst both::

        class MyToken(LexerToken):
            # Identifiers will keep an internalized version of the text
            Identifier = WithSymbol()
    """
    pass


class TokenFamily:
    """
    Set of tokens.

    All token families must form a partition on the set of tokens for a given
    lexer. They can then be used to define spacing rules for unparsing.
    """

    def __init__(self, *tokens):
        """
        :type tokens: list[TokenAction]
        """
        self.location = extract_library_location()
        self.tokens = set(tokens)

        self.name = None
        """
        Name for this family. Filled in LexerToken's constructor.

        :type: names.Name
        """

    @property
    def dsl_name(self):
        return self.name.camel

    @property
    def ada_name(self):
        return self.name.camel_with_underscores

    @property
    def signature(self):
        return ('TokenFamily', self.name.camel,
                sorted(t.signature for t in self.tokens))

    @property
    def diagnostic_context(self):
        return Context(
            'In definition of token family {}'.format(self.dsl_name),
            self.location
        )


class LexerToken:
    """
    Base class from which your token class must derive. Every member needs to
    be an instanciation of a subclass of TokenAction, specifiying what is done
    with the resulting token.
    """
    # Built-in termination token. Since it will always be the first token kind,
    # its value will always be zero.
    Termination = WithText()

    # Built-in token to represent a lexing failure. Consider them as trivia so
    # that we can try parsing ignoring them. Note that we need to emit a
    # diagnostic when they occur.
    LexingFailure = WithTrivia()

    def __init__(self, track_indent=False):
        import inspect

        if track_indent:
            self.__class__.Indent = WithText()
            self.__class__.Dedent = WithText()
            self.__class__.Newline = WithText()

        self.tokens = []
        """
        :type: list[TokenAction]
        """

        self.token_families = []
        """
        :type: list[TokenFamily]
        """

        self.token_to_family = {}
        """
        :type: dict[TokenAction, TokenFamily]
        """

        for c in inspect.getmro(self.__class__):
            self.add_tokens(c)

    def add_tokens(self, cls):
        for fld_name, fld_value in cls.__dict__.items():
            if isinstance(fld_value, TokenAction):
                dest_list = self.tokens
            elif isinstance(fld_value, TokenFamily):
                dest_list = self.token_families
            else:
                continue

            # Several items here are shared: for example, the
            # LexerToken.LexingFailure instance can be used in two different
            # lexers, so we can't assume its name is always None. Just accept
            # when it has already the expected name.
            name = Name.from_camel(fld_name)
            assert fld_value.name in (None, name)
            fld_value.name = name
            dest_list.append(fld_value)

    def __iter__(self):
        return (fld for fld in self.tokens)

    def __len__(self):
        return len(self.tokens)

    @property
    def signature(self):
        return ('LexerToken',
                sorted(t.signature for t in self.tokens),
                sorted(tf.signature for tf in self.token_families),
                sorted((t.name.camel, tf.name.camel)
                       for t, tf in self.token_to_family.items()))


class Lexer:
    """
    This is the main lexer object, through which you will define your Lexer.
    At initialization time, you will need to provide an enum class to it, that
    will be used to identify the different kinds of tokens that your lexer can
    generate. This is a simple example for a simple calculator's lexer::

        class Token(LexerToken):
            Plus = WithText()
            Minus = WithText()
            Times = WithText()
            Div = WithText()
            Number = WithText()

        l = Lexer(Token)

    You can add patterns to it, that are shortcuts to regex patterns, and that
    can refer to each others, like so::

        l.add_patterns(
            ('digit',   r"[0-9]"),
            ('integer', r"{digit}(_?{digit})*"),
        )

    Note that this is not necessary, just a convenient shortcut. After that
    you'll be able to define the match rules for your lexer, via the
    `add_rules` function::

        l.add_rules((
            (Literal("+"),         Token.Plus),
            (Literal("-"),         Token.Minus),
            (Literal("*"),         Token.Times),
            (Literal("/"),         Token.Div),
            (Pattern('{integer}'), Token.Number),
        ))

    After that, your lexer is complete! You can use it in your parser to
    generate parse trees.
    """

    def __init__(self, tokens_class, track_indent=False, pre_rules=[]):
        """
        :param type tokens_class: The class for the lexer's tokens.
        :param bool track_indent: Whether to track indentation when lexing or
            not. If this is true, then the special Layout parsers can be used
            to do indentation sensitive parsing.

        :param pre_rules: A list of rules to add before the built-in new-line
            rule, if track_indent is True. If track_indent is false, adding
            rules this way is the same as calling add_rules.
        :type pre_rules: list[(Matcher, Action)|RuleAssoc]
        """

        self.tokens = tokens_class(track_indent)
        assert isinstance(self.tokens, LexerToken)

        self.patterns = []
        self.rules = []
        self.tokens_set = {el.name for el in self.tokens}
        self.track_indent = track_indent

        # This map will keep a mapping from literal matches to token kind
        # values, so that you can find back those values if you have the
        # literal that corresponds to it.
        self.literals_map = {}

        # Map from token actions class names to set of token actions with that
        # class.
        self.token_actions = defaultdict(set)

        for el in self.tokens:
            self.token_actions[type(el).__name__].add(el)

        self.add_rules(*pre_rules)

        if self.track_indent:
            self.add_rules(
                (Literal('\n'), self.tokens.Newline),
            )

        self.spacing_table = defaultdict(lambda: defaultdict(lambda: False))
        """
        Nested mapping that indicates whether two tokens must be separated by a
        space during unparsing.

        A space must be inserted between two token T1 and T2 iff
        ``spacing_rules[T1.family][T2.family]`` is true.

        :type: dict[TokenFamily, dict[TokenFamily, bool]]
        """

        self.newline_after = set()
        """
        Set of tokens after which unparsing must emit a line break.

        :type: set[TokenAction]
        """

    @property
    def signature(self):
        return ('Lexer',
                self.track_indent,
                self.prefix,
                self.tokens.signature,

                sorted(p.signature for p in self.patterns),

                # Do not sort signatures for rules as their order matters
                [r.signature for r in self.rules],

                sorted((t1.name.camel,
                        sorted(t2.name.camel
                               for t2, present in mapping.items()
                               if present))
                       for t1, mapping in self.spacing_table.items()),

                sorted(tf.name.camel for tf in self.newline_after))

    def add_patterns(self, *patterns):
        r"""
        Add the list of named patterns to the lexer's internal patterns. A
        named pattern is a pattern that you can refer to through the {}
        notation in another pattern, or directly via the lexer instance::

            l.add_patterns(
                ('digit', r"[0-9]"),
                ('integer', r"({digit}(_?{digit})*)"),
            )

            l.add_rules(
                (l.patterns.integer, WithText(TokenKind.Number))
                (Pattern("{integer}(\.{integer})?"),
                 WithText(TokenKind.Number))
            )

        Please note that the order of addition matters if you want to refer to
        patterns in other patterns.

        :param list[(str, str)] patterns: The list of patterns to add.
        """
        loc = extract_library_location()
        for k, v in patterns:
            assert isinstance(k, str)
            assert isinstance(v, str)
            self.patterns.append((k, v, loc))

    def add_rules(self, *rules):
        """
        Add the list of rules to the lexer's internal list of rules. A rule is
        either:

        * A tuple of a Matcher and an Action to execute on this matcher. This
          is the common case;
        * An instance of a class derived from `RuleAssoc`. This is used to
          implement custom matching behaviour, such as in the case of `Case`.

        Please note that the order of addition matters. It will determine which
        rules are tried first by the lexer, so you could in effect make some
        rules 'dead' if you are not careful.

        :param rules: The list of rules to add.
        :type rules: list[(Matcher, Action)|RuleAssoc]
        """
        for matcher_assoc in rules:
            if type(matcher_assoc) is tuple:
                assert len(matcher_assoc) == 2
                matcher, action = matcher_assoc
                rule_assoc = RuleAssoc(matcher, action, matcher.location)
            else:
                assert isinstance(matcher_assoc, RuleAssoc)
                rule_assoc = matcher_assoc

            self.rules.append(rule_assoc)

            m, a = rule_assoc.matcher, rule_assoc.action

            if isinstance(m, (Literal, NoCaseLit)):
                # If the action is a case action, we'll take the send action of
                # the default alternative.
                if isinstance(a, Case.CaseAction):
                    a = a.default_alt.send

                # Add a mapping from the literal representation of the token to
                # itself, so that we can find tokens via their literal
                # representation.
                self.literals_map[m.to_match] = a

                a.matcher = m

    def add_spacing(self, *token_family_couples):
        """
        Add mandatory spacing rules for the given couples of token families.

        For each given token families TF1 and TF2, state that during unparsing,
        a token that belongs to TF1 must be followed by space when a token that
        belongs to TF2 comes next. By default, no space is inserted.

        :type token_family_couples: list[(TokenFamily, TokenFamily)]
        """
        for tf1, tf2 in token_family_couples:
            self.spacing_table[tf1][tf2] = True

    def add_newline_after(self, *tokens):
        """
        Add mandatory line break emission during unparsing after the given
        tokens.

        :type tokens: list[TokenAction]
        """
        self.newline_after.update(tokens)

    def build_dfa_code(self, context):
        """
        Build the DFA that implements this lexer (self.dfa_code).
        """
        assert context.nfa_start is not None

        def get_action(labels):
            # If this set of labels contain one or several actions, get the
            # most prioritary one and leave out the integer used to encode
            # priority. See compile_rules for how these integers are computed.
            sorted_actions = sorted(labels)
            return sorted_actions[0][1] if sorted_actions else None

        # Compute the corresponding DFA
        return DFACodeGenHolder(context.nfa_start.to_dfa(), get_action)

    def get_token(self, literal):
        """
        Helper function to get the name of a token.

        :param TokenAction|Enum|Name|str token: Input token. It can be either a
            TokenAction subclass (i.e. a Lexer subclass attribute), an enum
            value from "self.tokens", the token Name or a string (case
            insensitive token name).
        :rtype: Name
        """
        assert isinstance(literal, str), (
            "Bad type for {}, supposed to be str|{}".format(
                literal, self.tokens.__name__
            )
        )
        check_source_language(
            literal in self.literals_map,
            '{} token literal is not part of the valid tokens for this'
            ' this grammar'.format(repr(literal))
        )
        return self.literals_map[literal]

    @property
    def sorted_tokens(self):
        """
        Return the list of token types sorted by their corresponding numeric
        values.

        :rtype: list[TokenAction]
        """
        return sorted(self.tokens, key=lambda t: t.value)

    def __getattr__(self, attr):
        """
        Shortcut to get a TokenAction stored in self.tokens.
        """
        return getattr(self.tokens, attr)

    def check_token_families(self, context):
        """
        Pass that checks that either there are no defined token families, or
        that they form a partition of existing tokens.
        """
        def format_token_list(tokens):
            return ', '.join(sorted(
                t.dsl_name if isinstance(t, TokenAction) else str(t)
                for t in tokens
            ))

        # Sort token families by name to ensure legality checks and code
        # generation determinism.
        self.tokens.token_families.sort(key=lambda tf: tf.name)

        all_tokens = set(self.tokens)
        seen_tokens = set()

        for family in self.tokens.token_families:
            with family.diagnostic_context:
                not_tokens = family.tokens - all_tokens
                check_source_language(
                    not not_tokens,
                    'Invalid tokens: {}'.format(format_token_list(not_tokens))
                )

                already_seen_tokens = seen_tokens & family.tokens
                check_source_language(
                    not already_seen_tokens,
                    'Tokens must belong to one family exclusively: {}'
                    .format(format_token_list(already_seen_tokens))
                )

                seen_tokens.update(family.tokens)

        # Create a token family to host all tokens that are not associated with
        # a specific token family.
        default_family = TokenFamily(*list(all_tokens - seen_tokens))
        default_family.name = Name('Default_Family')
        self.tokens.token_families.append(default_family)

        # Make it easy to get the family a token belongs to
        for tf in self.tokens.token_families:
            for t in tf.tokens:
                self.tokens.token_to_family[t] = tf

    def compile_rules(self, context):
        """
        Pass to turn the lexer DSL into our internal regexp objects.
        """
        assert context.nfa_start is None

        regexps = RegexpCollection()

        # Import patterns into regexps
        for name, pattern, loc in self.patterns:
            with Context('In definition of lexer pattern {}'.format(name),
                         loc):
                regexps.add_pattern(name, pattern)

        # Now turn each rule into a NFA
        nfas = []

        for i, a in enumerate(self.rules):
            assert isinstance(a, RuleAssoc)

            # Check that actions never emit Termination and LexingFailure
            # tokens. These tokens are supposed to be emitted by the lexing
            # engine only.
            def check(token):
                check_source_language(
                    token not in (self.tokens.Termination,
                                  self.tokens.LexingFailure),
                    '{} is reserved for automatic actions only'
                    .format(token.dsl_name))

            if isinstance(a.action, Case.CaseAction):
                for alt in a.action.all_alts:
                    check(alt.send)
            elif isinstance(a.action, Ignore):
                pass
            else:
                assert isinstance(a.action, TokenAction)
                check(a.action)

            with Context('In definition of lexer rules', a.location):
                nfa_start, nfa_end = regexps.nfa_for(a.matcher.regexp)
            nfas.append(nfa_start)

            # The first rule that was added must have precedence when multiple
            # rules compete for the longest match. To implement this behavior,
            # we associate increasing ids to each token action.
            nfa_end.label = (i, a.action)

        # Create a big OR for all possible accepted patterns
        context.nfa_start = NFAState()
        for nfa in nfas:
            context.nfa_start.add_transition(None, nfa)


class Literal(Matcher):
    """
    Matcher. This matcher will match the string given in parameter,
    literally. This means that characters which would be special in a
    Pattern will be regular characters here::

        Pattern("a+")   # Matches one or more a
        Literal("a+")   # Matches "a" followed by "+"
    """
    def __init__(self, to_match, location=None):
        super().__init__(location)
        self.to_match = to_match

    @property
    def match_length(self):
        return len(self.to_match)

    @property
    def regexp(self):
        return re.escape(self.to_match)

    @property
    def signature(self):
        return ('Literal', self.to_match)


class NoCaseLit(Literal):
    """
    Same as Literal, but with case insensitivity.
    """

    @property
    def regexp(self):
        return ''.join(
            ('[{}{}]'.format(c.lower(), c.upper())
             if c.lower() != c.upper() else
             re.escape(c))
            for c in self.to_match
        )

    @property
    def signature(self):
        return ('NoCaseLiteral', self.to_match)


class Ignore(Action):
    """
    Action. Basically ignore the matched text.
    """

    @property
    def signature(self):
        return 'Ignore'


class RuleAssoc:
    """
    Base class for a matcher -> action association. This class should not be
    used directly, since you can provide a tuple to add_rules, that will be
    expanded to a RuleAssoc.
    """
    def __init__(self, matcher, action, location=None):
        self.matcher = matcher
        self.action = action
        self.location = location or extract_library_location()

    @property
    def signature(self):
        return ('RuleAssoc', self.matcher.signature, self.action.signature)


class Alt:
    """
    Holder class used to specify the alternatives to a Case rule. Can only
    be used in this context.
    """
    def __init__(self, prev_token_cond=None, send=None, match_size=None):
        self.prev_token_cond = prev_token_cond
        self.send = send
        self.match_size = match_size

    @property
    def signature(self):
        return (
            'Alt',
            [t.signature for t in self.prev_token_cond]
            if self.prev_token_cond else [],
            self.send.signature,
            self.match_size
        )


class Case(RuleAssoc):
    """
    Special rule association that enables dispatching the action depending
    on the previously parsed token. The canonical example is the one for
    which this class was added: in the Ada language, a tick character can be
    used either as the start of a character literal, or as an attribute
    expression.

    One way to disambiguate is by looking at the previous token. An
    attribute expression can only happen is the token to the left is an
    identifier or the "all" keyword. In the rest of the cases, a tick will
    correspond to a character literal, or be a lexing error.

    We can express that with the case rule this way::

        Case(Pattern("'.'"),
             Alt(prev_token_cond=(Token.Identifier, Token.All),
                 send=Token.Tick,
                 match_size=1),
             Alt(send=Token.Char, match_size=3)),

    If the previous token is an Identifier or an All, then we send
    Token.Tick, with a match size of 1. We need to specify that because if
    the lexer arrived here, it matched one tick, any char, and another tick,
    so it needs to rewind back to the first tick.

    Else, then we matched a regular character literal. We send it.
    """

    class CaseAction(Action):
        def __init__(self, match_length, *alts):
            super().__init__()
            self.location = extract_library_location()
            self.match_length = match_length

            for alt in alts:
                check_source_language(
                    isinstance(alt, Alt),
                    'Invalid alternative to Case matcher: {}'.format(alt)
                )
                check_source_language(
                    alt.match_size <= match_length,
                    'Match size for this Case alternative ({}) cannot be'
                    ' longer than the Case matcher ({} chars)'.format(
                        alt.match_size, match_length
                    )
                )

            check_source_language(
                alts[-1].prev_token_cond is None,
                "The last alternative to a case matcher "
                "must have no prev token condition"
            )

            self.alts = alts[:-1]
            self.default_alt = alts[-1]

        @property
        def all_alts(self):
            return list(self.alts) + [self.default_alt]

        @property
        def signature(self):
            return ('CaseAction', self.match_length,
                    sorted(alt.signature for alt in self.all_alts))

    def __init__(self, matcher, *alts):
        super().__init__(
            matcher, Case.CaseAction(matcher.match_length, *alts)
        )
