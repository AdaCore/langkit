"""
Generation of automatic unparsers for Langkit grammars.
"""

from __future__ import annotations

from collections import defaultdict
from io import StringIO
import itertools
import sys
from typing import IO, TYPE_CHECKING, overload

import funcy

from langkit.common import text_repr
from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType,
    Field,
    get_context,
    resolve_type,
)
from langkit.diagnostics import (
    Location, WarningSet, check_source_language, diagnostic_context, error
)
from langkit.lexer import Ignore, LexerToken, Literal, TokenAction
import langkit.names as names
from langkit.parsers import (
    Cut, Defer, DontSkip, List, ListSepExtra, Null, Opt, Or, Parser, Predicate,
    Skip, StopCut, _Extract, _Row, _Token, _Transform
)
from langkit.utils import not_implemented_error


if TYPE_CHECKING:
    from typing_extensions import Self


def unwrap(parser: Parser) -> Parser:
    """
    Strip DontSkip and Predicate parser layers.
    """
    while True:
        if isinstance(parser, DontSkip):
            parser = parser.subparser
        elif isinstance(parser, Predicate):
            parser = parser.parser
        else:
            return parser


class Unparser:
    """
    Abstract class for unparsers.
    """

    def dump(self, stream: IO[str] | None = None) -> None:
        """
        Print a debug representation of this unparser to the ``stream`` file.

        :param stream: Stream to emit debug representation to.  ``sys.stdout``
            is used if left to None.
        """
        stream = stream or sys.stdout
        self._dump(stream)

    def dumps(self) -> str:
        """
        Return a debug representation of this unparser.
        """
        result = StringIO()
        self.dump(result)
        return result.getvalue()

    def _dump(self, stream: IO[str]) -> None:
        """
        Concrete subclasses must override this to implement the dump method.
        """
        raise not_implemented_error(self, type(self)._dump)

    def combine(self, other: Self) -> Self:
        """
        Return an unparser that combines `self` and `other`.

        Both are assumed to be of the same type. If both contain contradictory
        information, emit a user error.

        :param other: Unparser to combine with ``self``. Must be an other
            instance of self's type.
        """
        assert isinstance(other, type(self)), (
            'Incompatible unparsers:\n{}\n... and...\n{}'.format(
                self.dumps(), other.dumps()
            )
        )
        return self._combine(other)

    def _combine(self, other: Self) -> Self:
        """
        Concrete subclasses must override this to implement the combine method.
        """
        raise not_implemented_error(self, type(self)._combine)

    def collect(self, unparsers: Unparsers) -> None:
        """
        Traverse all unparsers in ``self`` (in prefix order) and register the
        various unparsers that need to be collected in ``unparsers``. This
        de-duplicates token sequence unparsers.

        :param unparsers: Collection of unparsers to complete.
        """
        raise not_implemented_error(self, type(self).collect)


class TokenUnparser(Unparser):
    """
    Unparser for a token. The token text must be known.
    """

    # Internal counter to generate unique variable names
    _counter = itertools.count(0)

    def __init__(self, token: TokenAction, match_text: str | None = None):
        """
        :param token: Kind for the token to unparse.
        :param match_text: If there is no literal corresponding to ``token``,
            this must be a string used for unparsing. Otherwise, this must be
            None.
        """
        self.token = token
        self.is_termination = False
        self.is_lexing_failure = False

        matcher = token.matcher
        if token == LexerToken.Termination:
            self.is_termination = True
        elif token == LexerToken.LexingFailure:
            self.is_lexing_failure = True
        elif matcher is None:
            assert match_text is not None
            match_text = match_text
        else:
            assert match_text is None
            assert isinstance(matcher, Literal)
            match_text = matcher.to_match

        self._match_text = match_text
        self._var_name: names.Name | None = None

    @property
    def is_special(self) -> bool:
        """
        Return whether this unparser represents a special token, which should
        not make it to unparsing tables.
        """
        return self.is_termination or self.is_lexing_failure

    @property
    def match_text(self) -> str:
        assert self._match_text is not None, str(self)
        return self._match_text

    @overload
    @classmethod
    def from_parser(cls, parser: Parser | _Token) -> TokenUnparser: ...

    @overload
    @classmethod
    def from_parser(cls, parser: None) -> None: ...

    @classmethod
    def from_parser(
        cls,
        parser: Parser | _Token | None,
    ) -> TokenUnparser | None:
        """
        Create a token unparser out of a parser, assumed to parse a token.
        If ``parser`` is None, return None.

        :param parser: Token parser to analyze.
        """
        if parser is None:
            return None

        assert isinstance(parser, _Token)
        token = parser.val
        match_text = parser.match_text or None

        unparsers = get_context().unparsers
        key = (token, match_text)
        try:
            return unparsers.token_unparsers[key]
        except KeyError:
            result = cls(token, match_text)
            unparsers.token_unparsers[key] = result
            return result

    @staticmethod
    def equivalent(
        token1: TokenUnparser | None,
        token2: TokenUnparser | None,
    ) -> bool:
        """
        Return whether `token1` and `token2` are equivalent tokens.
        """
        if token1 is None or token2 is None:
            return token1 is token2
        return token1.dumps() == token2.dumps()

    @staticmethod
    def dump_or_none(token: TokenUnparser | None) -> str:
        """
        Return ``token.dumps()`` unless it is None.
        """
        return '<none>' if token is None else token.dumps()

    def _dump(self, stream: IO[str]) -> None:
        if self.is_termination:
            label = "<termination>"
        elif self.is_lexing_failure:
            label = "<lexing failure>"
        else:
            label = self.match_text
        stream.write(label)

    @property
    def text_repr(self) -> str:
        """
        Return an Ada string literal for the text to emit this token.
        """
        return text_repr(self.match_text)

    # Comparing tokens is done through
    # ``TokenSequenceUnparser.check_equivalence``, which is already good at
    # providing context for users in diagnostics, so deliberately not
    # overriding the "_combine" method.

    def __repr__(self) -> str:
        return 'Token {}'.format(repr(self.dumps()))

    @property
    def var_name(self) -> names.Name:
        """
        Name of the variable to hold this sequence of tokens in code
        generation.
        """
        if self._var_name is None:
            self._var_name = names.Name('Token_Unparser_{}'
                                        .format(next(self._counter)))
        return self._var_name


class TokenSequenceUnparser(Unparser):
    """
    Sequence of token unparsers.
    """

    def __init__(self, init_tokens: list[TokenUnparser] | None = None):
        """
        :param init_tokens: Optional list of tokens to start with.
        """
        self.tokens = []
        if init_tokens:
            self.tokens = [t for t in init_tokens if not t.is_special]

        self._serial_number: int | None = None
        self._var_name: names.Name | None = None

    def __repr__(self) -> str:
        return f"<TokenSequenceUnparser for {self.tokens}>"

    def _dump(self, stream: IO[str]) -> None:
        if self.tokens:
            stream.write(' '.join(t.dumps() for t in self.tokens))
        else:
            stream.write('<no token>')

    def __len__(self) -> int:
        return len(self.tokens)

    def __add__(self, other: TokenSequenceUnparser) -> TokenSequenceUnparser:
        """
        Return a new token sequence unparser that is the concatenation of
        ``self`` and ``other``.
        """
        return TokenSequenceUnparser(self.tokens + other.tokens)

    def append(self, token: TokenUnparser) -> None:
        """
        Append a token to this sequence.

        :param token: Token unparser to append.
        """
        if token.is_special:
            return
        self.tokens.append(token)

    def check_equivalence(
        self,
        sequence_name: str,
        other: TokenSequenceUnparser,
    ) -> None:
        """
        Emit a user diagnostic if `self` and `other` are not equivalent
        sequences of token unparsers.

        :param sequence_name: Name of the token sequences to compare, used in
            the diagnostic label.
        :param other: Sequence to compare to ``self``.
        """
        with diagnostic_context(Location.nowhere):
            check_source_language(
                len(self.tokens) == len(other.tokens) and
                all(TokenUnparser.equivalent(tok1, tok2)
                    for tok1, tok2 in zip(self.tokens, other.tokens)),

                'Inconsistent {}:'
                '\n  {}'
                '\nand:'
                '\n  {}'.format(sequence_name, self.dumps(), other.dumps())
            )

    @property
    def var_name(self) -> names.Name:
        """
        Name of the variable to hold this sequence of tokens in code
        generation.
        """
        if not self.tokens:
            return names.Name('Empty_Token_Sequence')

        if self._var_name is None:
            assert self._serial_number is not None
            self._var_name = names.Name('Token_Sequence_{}'
                                        .format(self._serial_number))
        return self._var_name


class NodeUnparser(Unparser):
    """
    Base class for parse node unparsers.
    """

    def __init__(self, node: ASTNodeType):
        """
        :param node: Parse node that this unparser handles.
        """
        self.node = node

        self.var_name = names.Name("Unparser_For") + self.node.kwless_raw_name
        """
        Name of the variable to hold this node unparser in code generation.
        """

    @staticmethod
    def from_parser(node: ASTNodeType, parser: Parser) -> NodeUnparser:
        """
        Given a parser that creates a specific type of parse node, return the
        corresponding unparser. Emit a user diagnostic if this transformation
        cannot be made.

        :param node: Parse node that `parser` emits.
        :param parser: Parser for which we want to create an unparser.
        """
        assert not node.abstract and not node.synthetic, (
            f"Invalid unparser request for {node.dsl_name} ({parser})"
        )
        parser = unwrap(parser)

        with parser.diagnostic_context:
            if node.is_token_node:
                return NodeUnparser._from_token_node_parser(node, parser)

            if isinstance(parser, _Transform):
                return NodeUnparser._from_transform_parser(node, parser)

            if isinstance(parser, List):
                check_source_language(
                    isinstance(parser.parser, (Defer, List, Null, Or,
                                               _Transform)),
                    'Unparsers generation require list parsers to directly'
                    ' build nodes for each list item'
                )
                return ListNodeUnparser(
                    node,
                    TokenUnparser.from_parser(parser.sep),
                    parser.extra,
                )

            if isinstance(parser, Opt):
                if parser._booleanize:
                    # This is a special parser: when the subparser succeeds,
                    # the "present" alternative is created to hold its result,
                    # otherwise the "absent" alternative is created (and no
                    # token are consumed).
                    #
                    # So in both cases, we create an unparser, but we emit
                    # tokens only for the "present" alternative.
                    result = RegularNodeUnparser(node)
                    qual_type = resolve_type(parser._booleanize)
                    if node is qual_type._alternatives_map['Present']:
                        NodeUnparser._emit_to_token_sequence(parser.parser,
                                                             result.pre_tokens)
                    return result

                else:
                    return NodeUnparser.from_parser(node, parser.parser)

            if isinstance(parser, Null):
                return NullNodeUnparser(node)

            error("Unsupported parser for unparsers generation")

    @staticmethod
    def _from_transform_parser(
        node: ASTNodeType,
        parser: Parser,
    ) -> NodeUnparser:
        """
        Helper for _from_parser. Turn the given _Transform parser into a
        RegularNodeUnparser instance.

        :param node: Parse node that `parser` emits.
        :param parser: Parser for which we want to create an unparser.
        """
        assert isinstance(parser, _Transform)
        assert isinstance(parser.parser, _Row)

        result = RegularNodeUnparser(node)
        subparsers = parser.parser.parsers
        next_field = 0

        def surrounding_inter_tokens() -> tuple[
            TokenSequenceUnparser,
            TokenSequenceUnparser | None,
        ]:
            """
            Considering the next field to process, return a tuple that contains
            the token sequence that precedes it, and the token sequence that
            succeeds it.
            """
            if next_field == 0:
                pre_tokens = result.pre_tokens
                post_tokens = (result.inter_tokens[0]
                               if len(result.field_unparsers) > 1 else
                               result.post_tokens)

            elif next_field < len(result.field_unparsers):
                pre_tokens = result.inter_tokens[next_field - 1]
                post_tokens = (result.inter_tokens[next_field]
                               if next_field < len(result.inter_tokens) else
                               result.post_tokens)

            else:
                assert next_field == len(result.field_unparsers)
                pre_tokens = result.post_tokens
                post_tokens = None

            return (pre_tokens, post_tokens)

        # Analyze subparser to split the parsed sequence of tokens into
        # pre-tokens, parsed fields, token sequences between parse fields, and
        # post-tokens.
        for i, subp in enumerate(subparsers):
            if subp.discard():
                tok_seq, _ = surrounding_inter_tokens()
                NodeUnparser._emit_to_token_sequence(subp, tok_seq)
            else:
                pre_tokens, post_tokens = surrounding_inter_tokens()
                assert post_tokens is not None
                NodeUnparser._emit_to_field_unparser(
                    subp, result.field_unparsers[next_field],
                    pre_tokens, post_tokens
                )
                next_field += 1

        return result

    @staticmethod
    def _from_token_node_parser(
        node: ASTNodeType,
        parser: Parser,
    ) -> TokenNodeUnparser:
        """
        Helper for _from_parser. Assuming ``node`` is a token node, turn the
        given ``parser`` into a TokenNodeUnparser instance.

        :param node: Parse node that `parser` emits.
        :param parser: Parser for which we want to create an unparser.
        """
        assert node.is_token_node

        accepted = False
        if isinstance(parser, _Transform):
            # All _Transform parsers contain a _Row subparser
            assert isinstance(parser.parser, _Row)

            # Previous validation passes ensure that parsers for token nodes
            # parse exactly one token, so the assertion below should stand.
            subparsers = parser.parser.parsers
            assert len(subparsers) == 1

            token_parser = subparsers[0]
            if isinstance(token_parser, _Token):
                accepted = True
                token_kind = token_parser.val
                if node.token_kind not in (None, token_kind):
                    assert isinstance(node.token_kind, TokenAction)
                    error(
                        'The {} token node can be associated to only one token'
                        ' kind: here we have {}, but we already had {}'.format(
                            node.dsl_name, token_kind.dsl_name,
                            node.token_kind.dsl_name
                        )
                    )
                node.token_kind = token_kind

        check_source_language(
            accepted,
            "Unsupported token node parser for unparsers generation, only"
            " direct token parsers are accepted"
        )
        return TokenNodeUnparser(node)

    @staticmethod
    def _split_extract(
        parser: Parser,
    ) -> tuple[TokenSequenceUnparser, Parser, TokenSequenceUnparser]:
        """
        Split ``_Extract`` parsers into three parts: a sequence of pre-tokens,
        the node parser in the middle, and a sequence of post-tokens.

        :param parser: _Extract parser to split.
        """
        assert isinstance(parser, _Extract)
        assert isinstance(parser.parser, _Row)
        subparsers = parser.parser.parsers
        index = parser.index

        pre_toks = TokenSequenceUnparser()
        for pre_parser in subparsers[:index]:
            NodeUnparser._emit_to_token_sequence(pre_parser, pre_toks)

        node_parser = subparsers[parser.index]

        post_toks = TokenSequenceUnparser()
        for post_parser in subparsers[index + 1:]:
            NodeUnparser._emit_to_token_sequence(post_parser, post_toks)

        return (pre_toks, node_parser, post_toks)

    @staticmethod
    def _emit_to_token_sequence(
        parser: Parser,
        token_sequence: TokenSequenceUnparser,
    ) -> None:
        """
        Turn the given parser into a sequence of tokens.

        Emit a user diagnostic if ``parser`` is a parser that does not parse
        exactly a constant sequence of tokens.

        :param parser: Parser to analyze.
        :param token_sequence: List into which this appends the sequence of
            tokens.
        """
        parser = unwrap(parser)

        if isinstance(parser, _Row):
            for subparser in parser.parsers:
                NodeUnparser._emit_to_token_sequence(subparser, token_sequence)

        elif isinstance(parser, _Token):
            token_sequence.append(TokenUnparser.from_parser(parser))

        elif isinstance(parser, Opt) and parser._is_error:
            NodeUnparser._emit_to_token_sequence(parser.parser, token_sequence)

        elif isinstance(parser, (DontSkip, Cut)):
            pass

        else:
            check_source_language(
                False,
                'Static sequence of tokens expected, but got: {}'.format(
                    parser
                )
            )

    @staticmethod
    def _emit_to_field_unparser(
        parser: Parser,
        field_unparser: FieldUnparser,
        pre_tokens: TokenSequenceUnparser,
        post_tokens: TokenSequenceUnparser,
    ) -> None:
        """
        Considering ``field_unparser`` as a field unparser we are in the
        process of elaborating, and ``pre_tokens`` and ``post_tokens`` as the
        token sequences that surround this field, extract information from the
        given ``parser`` to complete them.

        If ``parser`` is anything else than a Null parser, set
        ``field_unparser.always_absent`` to True.

        Emit a user diagnostic if ``parser`` is too complex for this analysis.

        :param parser: Parser to analyze.
        :param field_unparser: Field unparser to complete.
        :param pre_tokens: Token sequences to contain the list of tokens that
            appear before the field, whether or not the field is present.
            Tokens are inserted at the end of this sequence.
        :param post_tokens: Token sequences to contain the list of tokens that
            appear after the field, whether or not the field is present. Tokens
            are inserted at the beginning of this sequence.
        """
        parser = unwrap(parser)

        # As all fields are nodes, previous validation passes made sure that
        # `parser` yields a parse node (potentially a null one).

        if isinstance(parser, (Defer, List, Null, _Transform, StopCut)):
            # Field parsing goes directly to node creation, so there is no
            # pre/post sequences of tokens.
            field_unparser.always_absent = (field_unparser.always_absent and
                                            isinstance(parser, Null))

        elif isinstance(parser, Opt):
            if not parser._booleanize:
                assert isinstance(parser.type, ASTNodeType)
                # Because we are in an Opt parser, we now know that this field
                # is optional, so it can be absent.
                field_unparser.always_absent = False
                field_unparser.empty_list_is_absent = parser.type.is_list_type

                # Starting from here, tokens to be unparsed in
                # ``parser.parser`` must be unparsed iff the field is present,
                # so respectively prepend and append token sequences in the
                # recursion to the field unparser itself.
                pre_tokens = TokenSequenceUnparser()
                post_tokens = TokenSequenceUnparser()
                NodeUnparser._emit_to_field_unparser(
                    parser.parser, field_unparser, pre_tokens, post_tokens
                )
                field_unparser.pre_tokens = (pre_tokens +
                                             field_unparser.pre_tokens)
                field_unparser.post_tokens = (field_unparser.post_tokens +
                                              post_tokens)

        elif isinstance(parser, Or):
            # Just check that all subparsers create nodes, and thus that there
            # is nothing specific to do here: the unparser will just recurse on
            # this field.
            field_unparser.always_absent = False
            for subparser in parser.parsers:
                # It is never legal for Pick parsers to be direct operands of
                # Or parsers, as it means that pre-post tokens for this field
                # will depend on what Or alternative was taken (not decidable
                # for unparsers).
                if isinstance(subparser, _Extract):
                    with subparser.diagnostic_context:
                        error("Pick parser cannot appear as an Or subparser")

                # Named parsing rules always create nodes, so we don't need to
                # check Defer parsers. Skip parsers also create nodes, but most
                # importantly they trigger a parsing error, so unparsers can
                # ignore them.
                if not isinstance(subparser, (Defer, Skip)):
                    assert isinstance(subparser.type, ASTNodeType)
                    NodeUnparser.from_parser(subparser.type, subparser)

        elif isinstance(parser, _Extract):
            field_unparser.always_absent = False
            pre_toks, node_parser, post_toks = NodeUnparser._split_extract(
                parser)

            # Pre and post-tokens from this _Extract parser appear whether or
            # not the parsed field is present, so they go in ``pre_tokens`` and
            # ``post_tokens``, not in the field unparser itself.
            pre_tokens.tokens = pre_tokens.tokens + pre_toks.tokens
            post_tokens.tokens = post_toks.tokens + post_tokens.tokens
            NodeUnparser._emit_to_field_unparser(
                node_parser, field_unparser, pre_tokens, post_tokens
            )

        else:
            check_source_language(
                False, 'Unsupported parser for node field: {}' .format(parser))


class NullNodeUnparser(NodeUnparser):
    """
    Dummy node unparser, used when we try to build an unparser from a parser
    that takes no token and returns a null parse node.
    """

    def _dump(self, stream: IO[str]) -> None:
        stream.write('Unparser for {}: null\n'.format(self.node.dsl_name))

    # Null unparsers are not supposed to be combined with others, so
    # deliberately not overriding the "_combine" method.


class FieldUnparser(Unparser):
    """
    Unparser for a node field.
    """

    def __init__(self, node: ASTNodeType, field: Field):
        """
        :param node: The node for which we create this field unparser. Because
            of node inheritance, this can be different than `field.struct`.
        :param field: Parse field that this unparser handles.
        """
        self.node = node
        self.field = field

        self.always_absent = True
        """
        Whether this is a dummy entry, i.e. we created it from a Null parser.

        :type: bool
        """

        self.empty_list_is_absent = False
        """
        Whether this field is to be considered as absent when it is an empty
        list node.

        :type: bool
        """

        self.pre_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that precedes this field during (un)parsing.

        :type: TokenSequenceUnparser
        """

        self.post_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that follows this field during (un)parsing.

        :type: TokenSequenceUnparser
        """

    def _dump(self, stream: IO[str]) -> None:
        if self.empty_list_is_absent:
            stream.write('   [empty_list_is_absent]\n')
        stream.write('   if {}: {} [field] {}\n'.format(
            self.field.qualname,
            self.pre_tokens.dumps(),
            self.post_tokens.dumps(),
        ))

    def _combine(self, other: Self) -> Self:
        assert other.node == self.node
        assert other.field == self.field

        if self.always_absent:
            return other
        elif other.always_absent:
            return self
        else:
            self.pre_tokens.check_equivalence(
                'prefix tokens for {}'.format(self.field.qualname),
                other.pre_tokens
            )
            self.post_tokens.check_equivalence(
                'postfix tokens for {}'.format(self.field.qualname),
                other.post_tokens
            )
            return self

    def collect(self, unparsers: Unparsers) -> None:
        tok_seq_pool = unparsers.token_sequence_unparsers
        self.pre_tokens = tok_seq_pool.get_unique(self.pre_tokens)
        self.post_tokens = tok_seq_pool.get_unique(self.post_tokens)


class RegularNodeUnparser(NodeUnparser):
    """
    Unparser for "regular" nodes.

    In this context, "regular" means that this node can hahve fields: it's not
    a list and it's not a token node.
    """

    def __init__(self, node: ASTNodeType):
        """
        :param node: Parse node that this unparser handles.
        """
        super().__init__(node)

        parse_fields = self.node.get_parse_fields(
            predicate=lambda f: not f.null)

        self.pre_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that precedes this field during (un)parsing.
        """

        self.field_unparsers = [FieldUnparser(node, field)
                                for field in parse_fields]
        """
        List of field unparsers corresponding to this node's parse fields.
        """

        self.inter_tokens = [TokenSequenceUnparser()
                             for _ in range(len(parse_fields) - 1)]
        """
        List of token sequences, corresponding to tokens that appear between
        parse fields. Token sequence at index N materializes tokes that appear
        between between fields N-1 and N.
        """

        self.post_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that follows this field during (un)parsing.
        """

    def __repr__(self) -> str:
        return (
            f"<RegularNodeUnparser for {self.node.dsl_name},"
            f" pre_tokens={self.pre_tokens},"
            f" field_unparsers={self.field_unparsers}, "
            f" inter_tokens={self.inter_tokens}, "
            f" post_tokens={self.post_tokens}>"
        )

    @property
    def fields_unparser_var_name(self) -> names.Name:
        """
        Return the name of the variable in code generation to store the
        unparsing table for fields.
        """
        return (self.node.name + names.Name('Fields_Unparser_List')
                if self.field_unparsers else
                names.Name('Empty_Field_Unparser_List'))

    @property
    def zip_fields(self) -> list[tuple[FieldUnparser, TokenSequenceUnparser]]:
        """
        Zipped list of field unparsers and inter-field token sequences.
        """
        return funcy.lzip(self.field_unparsers,
                          [TokenSequenceUnparser()] + self.inter_tokens)

    def _dump(self, stream: IO[str]) -> None:
        stream.write('Unparser for {}:\n'.format(self.node.dsl_name))
        if self.pre_tokens:
            stream.write('   pre: {}\n'.format(self.pre_tokens.dumps()))
        for field_unparser, inter_tokens in self.zip_fields:
            stream.write('\n')
            if inter_tokens:
                stream.write('   tokens: {}\n'.format(inter_tokens.dumps()))
            field_unparser.dump(stream)
        if self.field_unparsers:
            stream.write('\n')
        if self.post_tokens:
            stream.write('   post: {}\n'.format(self.post_tokens.dumps()))

    def _combine(self, other: Self) -> Self:
        assert self.node == other.node
        assert len(self.field_unparsers) == len(other.field_unparsers)
        assert len(self.inter_tokens) == len(other.inter_tokens)

        self.pre_tokens.check_equivalence(
            'prefix tokens for {}'.format(self.node.dsl_name),
            other.pre_tokens
        )

        for i, (self_inter, other_inter) in enumerate(
                zip(self.inter_tokens, other.inter_tokens)
        ):
            field = self.field_unparsers[i].field
            self_inter.check_equivalence(
                'tokens after {}'.format(field.qualname),
                other_inter
            )

        self.post_tokens.check_equivalence(
            'postfix tokens for {}'.format(self.node.dsl_name),
            other.post_tokens
        )

        result = type(self)(self.node)
        result.pre_tokens = self.pre_tokens
        result.post_tokens = self.post_tokens
        result.inter_tokens = self.inter_tokens
        result.field_unparsers = [
            self_fu.combine(other_fu)
            for self_fu, other_fu in zip(self.field_unparsers,
                                         other.field_unparsers)
        ]
        return result

    def collect(self, unparsers: Unparsers) -> None:
        tok_seq_pool = unparsers.token_sequence_unparsers

        self.pre_tokens = tok_seq_pool.get_unique(self.pre_tokens)

        for field_unparser in self.field_unparsers:
            field_unparser.collect(unparsers)

        self.inter_tokens = [tok_seq_pool.get_unique(tok_seq)
                             for tok_seq in self.inter_tokens]

        self.post_tokens = tok_seq_pool.get_unique(self.post_tokens)


class ListNodeUnparser(NodeUnparser):
    """
    Unparser for list nodes.
    """

    def __init__(
        self,
        node: ASTNodeType,
        separator: TokenUnparser | None,
        extra: ListSepExtra,
    ):
        """
        :param node: Parse node that this unparser handles.
        :param separator: Unparser for the separator token, or None if this
            list allows no separator.
        """
        super().__init__(node)
        self.separator = separator
        self.extra = extra

    def __repr__(self) -> str:
        return (
            f"<ListNodeUnparser for {self.node.dsl_name},"
            f" separator={repr(self.separator)},"
            f" extra={self.extra.name}>"
        )

    def _dump(self, stream: IO[str]) -> None:
        stream.write('Unparser for {}:\n'.format(self.node.dsl_name))
        if self.separator:
            stream.write('   separator: {}\n'.format(self.separator.dumps()))
        stream.write('   extra: {}\n'.format(self.extra.name))

    def _combine(self, other: Self) -> Self:
        assert self.node == other.node
        with diagnostic_context(Location.nowhere):
            check_source_language(
                TokenUnparser.equivalent(self.separator, other.separator),
                'Inconsistent separation token for {}: {} and {}'.format(
                    self.node.dsl_name,
                    TokenUnparser.dump_or_none(self.separator),
                    TokenUnparser.dump_or_none(other.separator)
                )
            )
            check_source_language(
                self.extra == other.extra,
                "Inconsistent extra separation token for"
                f" {self.node.dsl_name}: {self.extra.name} and"
                f" {other.extra.name}",
            )
        return self

    def collect(self, unparsers: Unparsers) -> None:
        pass


class TokenNodeUnparser(NodeUnparser):
    """
    Unparser for token nodes.
    """

    def __repr__(self) -> str:
        return f"<TokenNodeUnparser for {self.node.dsl_name}"

    def _dump(self, stream: IO[str]) -> None:
        stream.write('Unparser for {}\n'.format(self.node.dsl_name))

    def _combine(self, other: Self) -> Self:
        assert self.node == other.node
        return self

    def collect(self, unparsers: Unparsers) -> None:
        pass


class TokenSequenceUnparserPool:
    """
    Helper to help removing redundant token sequence unparsers.
    """

    def __init__(self) -> None:
        self.pool: dict[tuple[TokenUnparser, ...], TokenSequenceUnparser] = {}
        self.finalized = False
        self.sorted: list[TokenSequenceUnparser]

    def __len__(self) -> int:
        return len(self.pool)

    def __getitem__(self, index: int) -> TokenSequenceUnparser:
        return self.sorted[index]

    def get_unique(
        self,
        token_seq: TokenSequenceUnparser,
    ) -> TokenSequenceUnparser:
        assert not self.finalized
        key = tuple(token_seq.tokens)
        try:
            return self.pool[key]
        except KeyError:
            self.pool[key] = token_seq
            return token_seq

    def finalize(self) -> None:
        self.sorted = sorted(
            self.pool.values(),
            key=lambda tok_seq: tuple(t.dumps() for t in tok_seq.tokens),
        )

        # Assign a unique identification number to token sequences for code
        # generation.
        for i, tok_seq in enumerate(self.sorted):
            tok_seq._serial_number = i

        self.finalized = True


class Unparsers:
    """
    Holder for the creation of unparsing tables.

    The end goal is to have one and only one non-ambiguous rule to unparse an
    AST type.
    """

    def __init__(self, context: CompileCtx):
        self.context = context

        self.nodes_to_rules: dict[ASTNodeType, list[Parser]] = defaultdict(
            list
        )

        self.unparsers: dict[ASTNodeType, list[NodeUnparser]] = defaultdict(
            list
        )
        """
        Map instead each node to the corresponding list of unparsers.
        Unparsers are built from parsers that create these nodes.
        """

        self.token_unparsers: dict[
            tuple[TokenAction, str | None],
            TokenUnparser
        ] = {}
        """
        Cache for created token unparsers. This avoids the emission of the same
        token unparser constant over and over in generated code.
        """

        self.token_sequence_unparsers: TokenSequenceUnparserPool = (
            TokenSequenceUnparserPool()
        )
        """
        Pool of all token sequence unparsers in the unparsing tables. Computed
        at the end of finalization to retain only the final ones
        (de-duplicated).
        """

    @property
    def sorted_token_unparsers(self) -> list[TokenUnparser]:
        """
        List of all token unparsers. Order is consistent across runs.
        """
        result = [t for t in self.token_unparsers.values() if not t.is_special]
        result.sort(key=lambda t: t.dumps())
        return result

    def compute(self, parser: Parser) -> None:
        """
        Map every AST node type to the set of parsers that return this type.

        If unparsers are requested, compute unparsers for all node-constructing
        sub-parsers in ``parser``.

        Also abort the generation of unparsers if the grammar contains parsing
        constructs we don't support with unparsers.

        :param parser: Parser combinator to analyze.
        """

        # Skip parsers generated for DontSkip. They don't generate any nodes,
        # so are not interesting in that context.
        if parser.is_dont_skip_parser:
            return

        def append(node: ASTNodeType, parser: Parser) -> None:
            self.nodes_to_rules[node].append(parser)
            if self.context.generate_unparser:
                self.unparsers[node].append(
                    NodeUnparser.from_parser(node, parser)
                )

        def compute_internal(p: Parser, toplevel: bool = True) -> None:
            # Skip parsers create nodes out of thin air in reaction to a
            # parsing error, so unparser logics must ignore them.
            if isinstance(p, Skip):
                pass

            elif isinstance(p, Opt) and p._booleanize:
                qual_type = resolve_type(p._booleanize)
                for alt_type in qual_type._alternatives:
                    append(alt_type, p)
                toplevel = False

            elif isinstance(p, (List, _Transform)):
                assert isinstance(p.type, ASTNodeType)
                append(p.type, p)
                toplevel = False

            elif isinstance(p, (Null, Or)):
                pass

            elif isinstance(p, _Extract):
                assert isinstance(p.parser, _Row)
                subparsers = p.parser.parsers

                # Reject information loss at the top level. As a special case,
                # allow that top-level "p" parses a node followed by a
                # termination token.
                check_source_language(
                    not self.context.generate_unparser or
                    not toplevel or
                    (len(p.parser.parsers) == 2 and
                        isinstance(subparsers[1], _Token) and
                        subparsers[1]._val == LexerToken.Termination),
                    'Top-level information loss prevents unparsers generation'
                )

            for c in p.children:
                compute_internal(c, toplevel)

        compute_internal(parser)

    def reject_parserless_nodes(
        self,
        context: CompileCtx,
        node: ASTNodeType,
    ) -> None:
        """
        Check that all concrete non-synthetic nodes have at least one
        associated parser.
        """
        with node.diagnostic_context:
            check_source_language(
                bool(self.nodes_to_rules.get(node))
                or node.abstract
                or node.synthetic,
                f"{node.dsl_name} is not synthetic nor abstract, so at least"
                " one parser must create it",
            )

    def check_nodes_to_rules(self, ctx: CompileCtx) -> None:
        """
        Check the results of the compute pass, to see if every node type only
        has one non ambiguous way of being unparsed, and assign a canonical
        representation to every node type.

        Combine all unparsers for each node, checking their consistency, and
        attach the result as ``node.unparser``.
        """
        from langkit.compiled_types import CompiledTypeRepo

        # Check if every non-abstract non-synthetic node has a corresponding
        # parser.
        for node_type in CompiledTypeRepo.astnode_types:
            with node_type.diagnostic_context:
                WarningSet.unused_node_type.warn_if(
                    node_type not in self.nodes_to_rules.keys()
                    and not node_type.abstract
                    and not node_type.synthetic
                    and not node_type.is_error_node,
                    '{} has no parser, and is marked neither abstract nor'
                    ' synthetic'.format(node_type.dsl_name)
                )

    def finalize(self, context: CompileCtx) -> None:
        """
        Pass to finalize the preparation of unparsers code generation.
        """
        if not self.context.generate_unparser:
            return

        assert self.context.lexer
        for rule_assoc in self.context.lexer.rules:
            if isinstance(rule_assoc.action, Ignore):
                with diagnostic_context(rule_assoc.action.location):
                    error(
                        'Ignore() tokens are incompatible with unparsers.'
                        ' Consider using WithTrivia() instead.'
                    )

        # Combine all unparsers for each node, except synthetic/error/abstract
        # nodes. Check that they are consistent. Iterate on all nodes first to
        # get deterministic iteration.
        for node in self.context.astnode_types:
            if node.abstract or node.synthetic or node.is_error_node:
                continue

            # Make sure we had at least one non-null unparser for every node
            unparsers = [u for u in self.unparsers[node]
                         if not isinstance(u, NullNodeUnparser)]
            with node.diagnostic_context:
                check_source_language(
                    bool(unparsers),
                    'No non-null unparser for non-synthetic node: {}'
                    .format(node.dsl_name)
                )

            combined = unparsers.pop(0)
            for unparser in unparsers:
                combined = combined.combine(unparser)
                assert type(unparser) == type(combined), (
                    'Incompatible unparsers:\n{}\n... and...\n{}'.format(
                        combined.dumps(), unparser.dumps()
                    )
                )
            node.unparser = combined
            node.unparser.collect(self)

        self.token_sequence_unparsers.finalize()
