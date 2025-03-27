"""
Generation of automatic unparsers for Langkit grammars.
"""

from __future__ import annotations

from collections import defaultdict
import enum
from io import StringIO
import itertools
import sys
from typing import IO, TYPE_CHECKING, overload

import funcy

from langkit.common import text_repr
from langkit.compile_context import CompileCtx, Verbosity
from langkit.compiled_types import ASTNodeType, Field
from langkit.diagnostics import (
    Location,
    WarningSet,
    check_source_language,
    error,
)
from langkit.lexer import Ignore, LexerToken, Literal, TokenAction
import langkit.names as names
from langkit.parsers import (
    Cut,
    Defer,
    Discard,
    DontSkip,
    Grammar,
    List,
    ListSepExtra,
    Null,
    Opt,
    Or,
    Parser,
    Predicate,
    Skip,
    StopCut,
    _Extract,
    _Row,
    _Token,
    _Transform,
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
        assert isinstance(
            other, type(self)
        ), "Incompatible unparsers:\n{}\n... and...\n{}".format(
            self.dumps(), other.dumps()
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
    def from_parser(
        cls,
        unparsers: Unparsers,
        parser: Parser | _Token,
    ) -> TokenUnparser: ...

    @overload
    @classmethod
    def from_parser(cls, unparsers: Unparsers, parser: None) -> None: ...

    @classmethod
    def from_parser(
        cls,
        unparsers: Unparsers,
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
        return "<none>" if token is None else token.dumps()

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
        return "Token {}".format(repr(self.dumps()))

    @property
    def var_name(self) -> names.Name:
        """
        Name of the variable to hold this sequence of tokens in code
        generation.
        """
        if self._var_name is None:
            self._var_name = names.Name(
                "Token_Unparser_{}".format(next(self._counter))
            )
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
            stream.write(" ".join(t.dumps() for t in self.tokens))
        else:
            stream.write("<no token>")

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

    def clone(self) -> TokenSequenceUnparser:
        """
        Return a shallow copy of this token sequence unparser.
        """
        return TokenSequenceUnparser(self.tokens)

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
        check_source_language(
            len(self.tokens) == len(other.tokens)
            and all(
                TokenUnparser.equivalent(tok1, tok2)
                for tok1, tok2 in zip(self.tokens, other.tokens)
            ),
            f"Inconsistent {sequence_name}:"
            f"\n  {self.dumps()}"
            "\nand:"
            f"\n  {other.dumps()}",
            location=Location.nowhere,
        )

    @property
    def var_name(self) -> names.Name:
        """
        Name of the variable to hold this sequence of tokens in code
        generation.
        """
        if not self.tokens:
            return names.Name("Empty_Token_Sequence")

        if self._var_name is None:
            assert self._serial_number is not None
            self._var_name = names.Name(
                "Token_Sequence_{}".format(self._serial_number)
            )
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
    def from_parser(
        unparsers: Unparsers,
        node: ASTNodeType,
        parser: Parser,
    ) -> NodeUnparser:
        """
        Given a parser that creates a specific type of parse node, return the
        corresponding unparser. Emit a user diagnostic if this transformation
        cannot be made.

        :param node: Parse node that `parser` emits.
        :param parser: Parser for which we want to create an unparser.
        """
        assert (
            not node.abstract and not node.synthetic
        ), f"Invalid unparser request for {node.dsl_name} ({parser})"
        parser = unwrap(parser)

        if node.is_token_node:
            return NodeUnparser._from_token_node_parser(node, parser)

        if isinstance(parser, _Transform):
            return NodeUnparser._from_transform_parser(unparsers, node, parser)

        if isinstance(parser, List):
            check_source_language(
                isinstance(parser.parser, (Defer, List, Null, Or, _Transform)),
                "Unparsers generation require list parsers to directly build"
                " nodes for each list item",
                location=parser.location,
            )
            return ListNodeUnparser(
                node,
                TokenUnparser.from_parser(unparsers, parser.sep),
                parser.extra,
            )

        if isinstance(parser, Opt):
            if parser._booleanize:
                # This is a special parser: when the subparser succeeds, the
                # "present" alternative is created to hold its result,
                # otherwise the "absent" alternative is created (and no token
                # are consumed).
                #
                # So in both cases, we create an unparser, but we emit tokens
                # only for the "present" alternative.
                result = RegularNodeUnparser(node)
                qual_type = parser._booleanize
                if node is qual_type._alternatives_map["Present"]:
                    NodeUnparser._emit_to_token_sequence(
                        unparsers, parser.parser, result.pre_tokens
                    )
                return result

            else:
                return NodeUnparser.from_parser(unparsers, node, parser.parser)

        if isinstance(parser, Null):
            return NullNodeUnparser(node)

        error(
            "Unsupported parser for unparsers generation",
            location=parser.location,
        )

    @staticmethod
    def _from_transform_parser(
        unparsers: Unparsers,
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

        # Analyze subparser to split the parsed sequence of tokens into
        # pre-tokens, parsed fields, token sequences between parse fields, and
        # post-tokens.
        for i, subp in enumerate(subparsers):
            if subp.discard:
                tok_seq, _ = result.surrounding_inter_tokens(next_field)
                NodeUnparser._emit_to_token_sequence(unparsers, subp, tok_seq)
            else:
                pre_tokens, post_tokens = result.surrounding_inter_tokens(
                    next_field
                )
                assert post_tokens is not None
                field_unparser = result.field_unparsers[next_field]
                field_unparser.kind = NodeUnparser._emit_to_field_unparser(
                    unparsers, subp, field_unparser, pre_tokens, post_tokens
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
                        "The {} token node can be associated to only one token"
                        " kind: here we have {}, but we already had {}".format(
                            node.dsl_name,
                            token_kind.dsl_name,
                            node.token_kind.dsl_name,
                        ),
                        location=token_parser.location,
                    )
                node.token_kind = token_kind

        check_source_language(
            accepted,
            "Unsupported token node parser for unparsers generation, only"
            " direct token parsers are accepted",
            location=parser.location,
        )
        return TokenNodeUnparser(node)

    @staticmethod
    def _split_extract(
        unparsers: Unparsers,
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
            NodeUnparser._emit_to_token_sequence(
                unparsers, pre_parser, pre_toks
            )

        node_parser = subparsers[parser.index]

        post_toks = TokenSequenceUnparser()
        for post_parser in subparsers[index + 1 :]:
            NodeUnparser._emit_to_token_sequence(
                unparsers, post_parser, post_toks
            )

        return (pre_toks, node_parser, post_toks)

    @staticmethod
    def _emit_to_token_sequence(
        unparsers: Unparsers,
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
                NodeUnparser._emit_to_token_sequence(
                    unparsers, subparser, token_sequence
                )

        elif isinstance(parser, _Token):
            token_sequence.append(TokenUnparser.from_parser(unparsers, parser))

        elif isinstance(parser, Opt) and parser._is_error:
            NodeUnparser._emit_to_token_sequence(
                unparsers, parser.parser, token_sequence
            )

        elif isinstance(parser, (DontSkip, Cut)):
            pass

        else:
            error(
                f"Static sequence of tokens expected, but got: {parser}",
                location=parser.location,
            )

    @staticmethod
    def _emit_to_field_unparser(
        unparsers: Unparsers,
        parser: Parser,
        field_unparser: FieldUnparser,
        pre_tokens: TokenSequenceUnparser,
        post_tokens: TokenSequenceUnparser,
    ) -> FieldUnparserKind:
        """
        Considering ``field_unparser`` as a field unparser we are in the
        process of elaborating, and ``pre_tokens`` and ``post_tokens`` as the
        token sequences that surround this field, extract information from the
        given ``parser`` to complete them.

        Return the kind of field unparser that corresponds to ``parser``.

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

        def kind_from_nullable() -> FieldUnparserKind:
            """
            Return the field unparser kind that correspond to ``parser``.
            """
            if isinstance(parser, Null):
                return FieldUnparserKind.always_absent
            elif unparsers.nullable_parser[parser]:
                return FieldUnparserKind.maybe_absent
            else:
                return FieldUnparserKind.never_absent

        # As all fields are nodes, previous validation passes made sure that
        # `parser` yields a parse node (potentially a null one).

        if isinstance(parser, Null):
            return FieldUnparserKind.always_absent

        elif isinstance(parser, (Defer, List, _Transform, StopCut)):
            # Field parsing goes directly to node creation, so there is no
            # pre/post sequences of tokens.
            return kind_from_nullable()

        elif isinstance(parser, Opt):
            if not parser._booleanize:
                assert isinstance(parser.type, ASTNodeType)

                # Because we are in an Opt parser, we now know that this field
                # is optional, so it can be absent.
                field_unparser.empty_list_is_absent = parser.type.is_list_type

                # Starting from here, tokens to be unparsed in
                # ``parser.parser`` must be unparsed iff the field is present,
                # so respectively prepend and append token sequences in the
                # recursion to the field unparser itself.
                pre_tokens = TokenSequenceUnparser()
                post_tokens = TokenSequenceUnparser()
                NodeUnparser._emit_to_field_unparser(
                    unparsers,
                    parser.parser,
                    field_unparser,
                    pre_tokens,
                    post_tokens,
                )
                field_unparser.pre_tokens = (
                    pre_tokens + field_unparser.pre_tokens
                )
                field_unparser.post_tokens = (
                    field_unparser.post_tokens + post_tokens
                )

                if unparsers.nullable_parser[parser.parser] and (
                    field_unparser.pre_tokens or field_unparser.post_tokens
                ):
                    error(
                        f"field parser for {field_unparser.field.qualname} may"
                        " yield a null node, so unparsers cannot decide when"
                        " to include tokens associated to that field",
                        location=parser.location,
                    )

                return kind_from_nullable()

        elif isinstance(parser, Or):
            # Just check that all subparsers create nodes, and thus that there
            # is nothing specific to do here: the unparser will just recurse on
            # this field.
            for subparser in parser.parsers:
                # It is never legal for Pick parsers to be direct operands of
                # Or parsers, as it means that pre-post tokens for this field
                # will depend on what Or alternative was taken (not decidable
                # for unparsers).
                if isinstance(subparser, _Extract):
                    error(
                        "Pick parser cannot appear as an Or subparser",
                        location=subparser.location,
                    )

                # Named parsing rules always create nodes, so we don't need to
                # check Defer parsers. Skip parsers also create nodes, but most
                # importantly they trigger a parsing error, so unparsers can
                # ignore them.
                if not isinstance(subparser, (Defer, Skip)):
                    assert isinstance(subparser.type, ASTNodeType)
                    NodeUnparser.from_parser(
                        unparsers, subparser.type, subparser
                    )

            return kind_from_nullable()

        elif isinstance(parser, _Extract):
            pre_toks, node_parser, post_toks = NodeUnparser._split_extract(
                unparsers, parser
            )

            # Pre and post-tokens from this _Extract parser appear whether or
            # not the parsed field is present, so they go in ``pre_tokens`` and
            # ``post_tokens``, not in the field unparser itself.
            pre_tokens.tokens = pre_tokens.tokens + pre_toks.tokens
            post_tokens.tokens = post_toks.tokens + post_tokens.tokens
            return NodeUnparser._emit_to_field_unparser(
                unparsers, node_parser, field_unparser, pre_tokens, post_tokens
            )

        else:
            error(
                f"Unsupported parser for node field: {parser}",
                location=parser.location,
            )


class NullNodeUnparser(NodeUnparser):
    """
    Dummy node unparser, used when we try to build an unparser from a parser
    that takes no token and returns a null parse node.
    """

    def _dump(self, stream: IO[str]) -> None:
        stream.write("Unparser for {}: null\n".format(self.node.dsl_name))

    # Null unparsers are not supposed to be combined with others, so
    # deliberately not overriding the "_combine" method.


class FieldUnparserKind(enum.Enum):
    always_absent = enum.auto()
    """
    Unparser for a field that is known to be always absent.

    Such field unparsers cannot have pre/post tokens.
    """

    maybe_absent = enum.auto()
    """
    Unparser for a field that can be absent and present.

    Such field unparsers can have pre/post tokens: they are included in the
    unparsing iff the field is present.
    """

    never_absent = enum.auto()
    """
    Unparser for a field that is known to be always present.

    Such field unparsers cannot have pre/post tokens.
    """

    @staticmethod
    def combine(
        left: FieldUnparserKind,
        right: FieldUnparserKind,
    ) -> FieldUnparserKind:
        """
        Return the kind of field unparser to create when combining two field
        unparsers.
        """
        if (
            FieldUnparserKind.maybe_absent in (left, right)
            or (
                left == FieldUnparserKind.always_absent
                and right == FieldUnparserKind.never_absent
            )
            or (
                left == FieldUnparserKind.never_absent
                and right == FieldUnparserKind.always_absent
            )
        ):
            return FieldUnparserKind.maybe_absent
        else:
            assert left == right
            return left


class FieldUnparser(Unparser):
    """
    Unparser for a node field.
    """

    def __init__(
        self,
        node_unparser: RegularNodeUnparser,
        field: Field,
        field_index: int,
    ):
        """
        :param node_unparser: The regular node unparser for which we create
            this field unparser.
        :param field: Parse field that this unparser handles.
        :param field_index: 0-based index of his field in its parent node's
            list of children.
        """
        self.node_unparser = node_unparser
        self.node = node_unparser.node
        self.field = field
        self.field_index = field_index

        # Assign a dummy kind for this unparser: the kind generally cannot be
        # known at construct time. It will be computed later (see
        # _emit_to_field_unparser).
        self.kind = FieldUnparserKind.always_absent

        self.empty_list_is_absent = False
        """
        Whether this field is to be considered as absent when it is an empty
        list node.
        """

        self.pre_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that precedes this field during (un)parsing.
        """

        self.post_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that follows this field during (un)parsing.
        """

    @staticmethod
    def assert_same_field(left: FieldUnparser, right: FieldUnparser) -> None:
        """
        Assert that ``left`` and ``right`` are unparsers for the same field.
        """
        assert left.node == right.node
        assert left.field == right.field
        assert left.field_index == right.field_index

    def _dump(self, stream: IO[str]) -> None:
        name = self.field.qualname
        if self.pre_tokens or self.post_tokens:
            pre_repr = f"{self.pre_tokens.dumps()} " if self.pre_tokens else ""
            post_repr = (
                f" {self.post_tokens.dumps()}" if self.post_tokens else ""
            )
            stream.write(f"   if {name}: {pre_repr}[field]{post_repr}")
        else:
            stream.write(f"   {name}")
        if self.empty_list_is_absent:
            stream.write(" # empty_list_is_absent")
        stream.write("\n")

    def clone(self) -> FieldUnparser:
        """
        Return a clone for this regular node unparser.

        The result is a "moderately" deep copy: the token sequence unparsers it
        contains are copied too, but not the token unparsers (treated as
        immutable constants).
        """
        result = FieldUnparser(
            self.node_unparser, self.field, self.field_index
        )
        result.kind = self.kind
        result.empty_list_is_absent = self.empty_list_is_absent
        result.pre_tokens = self.pre_tokens.clone()
        result.post_tokens = self.post_tokens.clone()
        return result

    def _combine(self, other: FieldUnparser) -> FieldUnparser:
        assert other.node == self.node
        assert other.field == self.field

        self.pre_tokens.check_equivalence(
            "prefix tokens for {}".format(self.field.qualname),
            other.pre_tokens,
        )
        self.post_tokens.check_equivalence(
            "postfix tokens for {}".format(self.field.qualname),
            other.post_tokens,
        )

        # The result must have the right kind for the combination. Create a
        # clone if neither "self" nor "other" have the kind we need.
        result_kind = FieldUnparserKind.combine(self.kind, other.kind)
        if self.kind == result_kind:
            return self
        elif other.kind == result_kind:
            return other
        else:
            result = self.clone()
            result.kind = result_kind
            return result

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
            predicate=lambda f: not f.null
        )

        self.pre_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that precedes this field during (un)parsing.
        """

        self.field_unparsers = [
            FieldUnparser(self, field, i)
            for i, field in enumerate(parse_fields)
        ]
        """
        List of field unparsers corresponding to this node's parse fields.
        """

        self.inter_tokens = [
            TokenSequenceUnparser() for _ in range(len(parse_fields) - 1)
        ]
        """
        List of token sequences, corresponding to tokens that appear between
        parse fields. Token sequence at index N materializes tokes that appear
        between between fields N-1 and N.
        """

        self.post_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that follows this field during (un)parsing.
        """

    def clone(self) -> RegularNodeUnparser:
        """
        Return a clone for this regular node unparser.

        The result is a "moderately" deep copy: the field unparsers and token
        sequence unparsers it contains are copied too, but not the token
        unparsers (treated as immutable constants).
        """
        result = RegularNodeUnparser(self.node)
        result.pre_tokens = self.pre_tokens.clone()
        result.field_unparsers = [f.clone() for f in self.field_unparsers]
        for f in result.field_unparsers:
            f.node_unparser = result
        result.inter_tokens = [t.clone() for t in self.inter_tokens]
        result.post_tokens = self.post_tokens.clone()
        return result

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
        return (
            self.node.name + names.Name("Fields_Unparser_List")
            if self.field_unparsers
            else names.Name("Empty_Field_Unparser_List")
        )

    @property
    def zip_fields(self) -> list[tuple[FieldUnparser, TokenSequenceUnparser]]:
        """
        Zipped list of field unparsers and inter-field token sequences.
        """
        return funcy.lzip(
            self.field_unparsers, [TokenSequenceUnparser()] + self.inter_tokens
        )

    def surrounding_inter_tokens(self, field_index: int) -> tuple[
        TokenSequenceUnparser,
        TokenSequenceUnparser | None,
    ]:
        """
        Considering the field at the given index, return a tuple that contains
        the token sequence that precedes it, and the token sequence that
        succeeds it.
        """
        if field_index == 0:
            pre_tokens = self.pre_tokens
            post_tokens = (
                self.inter_tokens[0]
                if len(self.field_unparsers) > 1
                else self.post_tokens
            )

        elif field_index < len(self.field_unparsers):
            pre_tokens = self.inter_tokens[field_index - 1]
            post_tokens = (
                self.inter_tokens[field_index]
                if field_index < len(self.inter_tokens)
                else self.post_tokens
            )

        else:
            assert field_index == len(self.field_unparsers)
            pre_tokens = self.post_tokens
            post_tokens = None

        return (pre_tokens, post_tokens)

    def adjust_null_to_maybe(
        self,
        fu1: FieldUnparser,
        fu2: FieldUnparser,
    ) -> None:
        """
        If given one "maybe absent" and one "always absent" field unparser
        (interchangeably "fu1" and "fu2"), adjust the "always absent" one to be
        compatible with the "maybe absent" one. This modifies the "always
        absent" field unparser in-place.
        """
        FieldUnparser.assert_same_field(fu1, fu2)

        def can_adjust(
            maybe_absent: FieldUnparser,
            always_absent: FieldUnparser,
        ) -> bool:
            """
            Return whether we can adjust "always_absent" to be optional like
            "maybe_absent".
            """
            return (
                maybe_absent.kind == FieldUnparserKind.maybe_absent
                and always_absent.kind == FieldUnparserKind.always_absent
            )

        # If we have one "maybe present" field and an "always present"
        # field, we must adapt the latter to the former.
        if can_adjust(fu1, fu2):
            field_maybe_absent = fu1
            field_always_absent = fu2
        elif can_adjust(fu2, fu1):
            field_maybe_absent = fu2
            field_always_absent = fu1
        else:
            return

        assert not field_always_absent.pre_tokens.tokens
        assert not field_always_absent.post_tokens.tokens
        field_always_absent.kind = FieldUnparserKind.maybe_absent
        field_always_absent.pre_tokens = field_maybe_absent.pre_tokens.clone()
        field_always_absent.post_tokens = (
            field_maybe_absent.post_tokens.clone()
        )

    def adjust_never_to_maybe(
        self,
        fu1: FieldUnparser,
        fu2: FieldUnparser,
    ) -> None:
        """
        If given one "maybe absent" and one "never absent" field unparser
        (interchangeably "fu1" and "fu2"), adjust the "never absent" one to be
        compatible with the "maybe absent" one. This modifies the "never
        absent" field unparser in-place as well as surrounding tokens in its
        parent node unparser.

        An example to clarify. Assume we have two unparsers for node type N
        (which has a single field "f")::

           # This one considers that field f1 is mandatory
           N["tok1", "tok2", f1, "tok3"]

           # This one considers that field f1 is optional, and that it is only
           # when that field is present that "tok2" and "tok3" must be
           # unparsed.
           N["tok1", ?("tok2", f1, "tok3"]

        Our goal here is to adapt the first unparser to make "f1", "tok2", and
        "tok3" optional. We also want to reject situations for incompatible
        unparsers, such as::

           # "tok2" is missing
           N["tok1", f1, "tok3"]
        """
        FieldUnparser.assert_same_field(fu1, fu2)
        field_index = fu1.field_index

        def can_adjust(
            maybe_absent: FieldUnparser,
            never_absent: FieldUnparser,
        ) -> bool:
            """
            Return whether we can adjust "never_absent" to be optional like
            "maybe_absent".
            """
            return (
                maybe_absent.kind == FieldUnparserKind.maybe_absent
                and never_absent.kind == FieldUnparserKind.never_absent
            )

        # If we have one "maybe present" field and an "always present"
        # field, we must adapt the latter to the former.
        if can_adjust(fu1, fu2):
            field_maybe_absent = fu1
            field_never_absent = fu2
        elif can_adjust(fu2, fu1):
            field_maybe_absent = fu2
            field_never_absent = fu1
        else:
            return

        assert not field_never_absent.pre_tokens.tokens
        assert not field_never_absent.post_tokens.tokens

        # Get the tokens unparser that surround the mandatory field: there,
        # we expect to find the optional tokens associated to the optional
        # field.
        before_never_absent, _after_never_absent = (
            field_never_absent.node_unparser.surrounding_inter_tokens(
                field_index
            )
        )
        after_never_absent = _after_never_absent or TokenSequenceUnparser()

        # Check the presence of expected surrounding tokens
        before_count = len(field_maybe_absent.pre_tokens.tokens)
        before_slice = TokenSequenceUnparser(
            before_never_absent.tokens[-before_count:] if before_count else []
        )
        after_count = len(field_maybe_absent.post_tokens.tokens)
        after_slice = TokenSequenceUnparser(
            after_never_absent.tokens[:after_count] if after_count else []
        )

        before_slice.check_equivalence(
            f"pre-surrounding tokens for {field_maybe_absent.field.qualname}",
            field_maybe_absent.pre_tokens,
        )
        after_slice.check_equivalence(
            f"post-surrounding tokens for {field_maybe_absent.field.qualname}",
            field_maybe_absent.post_tokens,
        )

        # Transfer expected surrounding tokens (in before|after_never_absent)
        # to the field unparser (field_never_absent.pre|post_tokens).
        if before_count:
            field_never_absent.pre_tokens.tokens[:] = (
                before_never_absent.tokens[-before_count:]
            )
            before_never_absent.tokens[-before_count:] = []
        if after_count:
            field_never_absent.post_tokens.tokens[:] = (
                after_never_absent.tokens[:after_count]
            )
            after_never_absent.tokens[:after_count] = []

        # The "never absent" field unparser has been adapted to be compatible
        # with the "maybe absent" one, and that made it a "maybe absent"
        # unparser in the process: update its kind accordingly.
        field_never_absent.kind = FieldUnparserKind.maybe_absent

    def _dump(self, stream: IO[str]) -> None:
        stream.write("Unparser for {}: regular\n".format(self.node.dsl_name))
        if self.pre_tokens:
            stream.write("   pre: {}\n".format(self.pre_tokens.dumps()))
        for field_unparser, inter_tokens in self.zip_fields:
            if inter_tokens:
                stream.write("   tokens: {}\n".format(inter_tokens.dumps()))
            field_unparser.dump(stream)
        if self.post_tokens:
            stream.write("   post: {}\n".format(self.post_tokens.dumps()))

    def _combine(self, other: RegularNodeUnparser) -> RegularNodeUnparser:
        assert self.node == other.node
        assert len(self.field_unparsers) == len(other.field_unparsers)
        assert len(self.inter_tokens) == len(other.inter_tokens)

        # We may mutate the unparsers to combine, so work on copies
        left = self.clone()
        right = other.clone()

        # Try to rework pairs of unparsers so that field unparsers can be
        # combined.
        #
        # Note: we do this here rather than in FieldUnparser.combine because
        # the logic is not local to a single field unparser: we need to deal
        # with tokens that surround field unparsers.
        for field_index, (left_field, right_field) in enumerate(
            zip(left.field_unparsers, right.field_unparsers)
        ):
            self.adjust_null_to_maybe(left_field, right_field)
            self.adjust_never_to_maybe(left_field, right_field)

        # Now that the "optional adaptations" are done, check equivalence
        # between our copies and return one of them.

        left.pre_tokens.check_equivalence(
            "prefix tokens for {}".format(left.node.dsl_name), right.pre_tokens
        )

        for i, (left_inter, right_inter) in enumerate(
            zip(left.inter_tokens, right.inter_tokens)
        ):
            field = left.field_unparsers[i].field
            left_inter.check_equivalence(
                "tokens after {}".format(field.qualname), right_inter
            )

        left.post_tokens.check_equivalence(
            "postfix tokens for {}".format(left.node.dsl_name),
            right.post_tokens,
        )

        left.field_unparsers = [
            left_fu.combine(right_fu)
            for left_fu, right_fu in zip(
                left.field_unparsers, right.field_unparsers
            )
        ]
        return left

    def collect(self, unparsers: Unparsers) -> None:
        tok_seq_pool = unparsers.token_sequence_unparsers

        self.pre_tokens = tok_seq_pool.get_unique(self.pre_tokens)

        for field_unparser in self.field_unparsers:
            field_unparser.collect(unparsers)

        self.inter_tokens = [
            tok_seq_pool.get_unique(tok_seq) for tok_seq in self.inter_tokens
        ]

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
        stream.write("Unparser for {}: list\n".format(self.node.dsl_name))
        if self.separator:
            stream.write("   separator: {}\n".format(self.separator.dumps()))
        stream.write("   extra: {}\n".format(self.extra.name))

    def _combine(self, other: Self) -> Self:
        assert self.node == other.node
        check_source_language(
            TokenUnparser.equivalent(self.separator, other.separator),
            "Inconsistent separation token for {}: {} and {}".format(
                self.node.dsl_name,
                TokenUnparser.dump_or_none(self.separator),
                TokenUnparser.dump_or_none(other.separator),
            ),
            location=Location.nowhere,
        )
        check_source_language(
            self.extra == other.extra,
            "Inconsistent extra separation token for"
            f" {self.node.dsl_name}: {self.extra.name} and"
            f" {other.extra.name}",
            location=Location.nowhere,
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
        stream.write(f"Unparser for {self.node.dsl_name}: token\n")

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

        self.nullable_parser: dict[Parser, bool] = {}
        """
        For each node-returning parser, whether it may return a null node or an
        empty list.
        """

        self.unparsers: dict[ASTNodeType, list[NodeUnparser]] = defaultdict(
            list
        )
        """
        Map instead each node to the corresponding list of unparsers.
        Unparsers are built from parsers that create these nodes.
        """

        self.token_unparsers: dict[
            tuple[TokenAction, str | None], TokenUnparser
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

    def compute_nullability(self, grammar: Grammar, ctx: CompileCtx) -> None:
        """
        Compute nullability for all parsers in the given grammar, i.e. for all
        node-returning parsers, whether they may yield a null node (or an empty
        list). The resulting nullability map is assigned to
        ``self.nullable_parser``.
        """
        # When the debug verbosity level is active, trace the nullability
        # computation.
        debug = ctx.verbosity == Verbosity.DEBUG

        # Nullability map that we intend to build. None values mean that the
        # stack of calls to "recurse" is currently processing a parser, so that
        # we do not run into infinite recursions for recursive rules.
        nullable: dict[Parser, bool | None] = {}

        # Queue of parsers for which we want to compute nullability
        queue: list[Parser] = []

        def node_returning(p: Parser) -> bool:
            """
            Return whether the given parser yields nodes.
            """
            return not isinstance(p, (Cut, Discard, _Row, _Token))

        def enqueue(p: Parser) -> None:
            """
            If nullability for ``p`` is still to be computed, add it to the
            queue.
            """
            assert node_returning(p), "Trying to enqueue parser {p}"
            if p not in nullable:
                queue.append(p)

        def enqueue_row(p: _Row) -> None:
            """
            Enqueue all unparsers in the given row.
            """
            for sp in p.parsers:
                if node_returning(sp):
                    enqueue(sp)

        def set(p: Parser, n: bool) -> bool:
            """
            Helper for concise code: set nullability for ``p`` to ``n`` and
            return ``n``.
            """
            nullable[p] = n
            return n

        def recurse(p: Parser) -> bool:
            """
            Compute and return the nullability for the given parser.
            """
            # Return the result if we already know it
            try:
                result = nullable[p]
            except KeyError:
                pass
            else:
                # Be conservative in case of infinite recursion: assume
                # nullability.
                return result is None or result

            nullable.setdefault(p, None)
            match p:
                case Defer():
                    return set(p, recurse(p.parser))

                case DontSkip():
                    return set(p, recurse(p.subparser))

                case List():
                    enqueue(p.parser)
                    if debug and p.empty_valid:
                        print(f"{p} is nullable because empty_valid")
                    return set(p, p.empty_valid)

                case Null():
                    return set(p, True)

                case Opt():
                    if isinstance(p.parser, _Row):
                        enqueue_row(p.parser)
                    elif node_returning(p.parser):
                        enqueue(p.parser)

                    # If this Opt parser creates an error if the subparser
                    # fails. Since nullability assumes no parsing error, we can
                    # consider that "is_error" Opt parsers never return null
                    # nodes.
                    #
                    # Booleanized Opt parsers never return null nodes either.
                    return set(p, not p._booleanize and not p._is_error)

                case Or():
                    # Or is nullable if at least one of its subparsers is
                    # nullable. Do not stop if we find one so that nullability
                    # is computed for all subparsers.
                    result = False
                    for sp in p.parsers:
                        if node_returning(sp) and recurse(sp):
                            if debug:
                                print(
                                    f"{p} is nullable because {sp} is nullable"
                                )
                            result = True
                    return set(p, result)

                case Predicate():
                    return set(p, recurse(p.parser))

                case Skip():
                    return set(p, False)

                case StopCut():
                    return set(p, recurse(p.parser))

                case _Extract():
                    return set(p, recurse(p.parser.parsers[p.index]))

                case _Transform():
                    enqueue_row(p.parser)
                    return set(p, False)

                case Cut() | _Row() | _Token():
                    raise AssertionError(
                        f"unreachable code: computing nullability for {p}"
                    )

                case _:
                    raise AssertionError(f"unhandled parser type {p}")

        for _, p in sorted(grammar.rules.items()):
            enqueue(p)
        while queue:
            p = queue.pop()
            recurse(p)

        for p, n in nullable.items():
            assert n is not None
            self.nullable_parser[p] = n

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
            if self.context.generate_unparsers:
                self.unparsers[node].append(
                    NodeUnparser.from_parser(self, node, parser)
                )

        def compute_internal(p: Parser, toplevel: bool = True) -> None:
            # Skip parsers create nodes out of thin air in reaction to a
            # parsing error, so unparser logics must ignore them.
            if isinstance(p, Skip):
                pass

            elif isinstance(p, Opt) and p._booleanize:
                qual_type = p._booleanize
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

                # Reject information loss at the top level. Accept two special
                # cases: termination tokens and cut parsers (no info loss for
                # these).
                if self.context.generate_unparsers and toplevel:

                    def ignore_subparser(p: Parser) -> bool:
                        match p:
                            case _Token(_val=LexerToken.Termination) | Cut():
                                return True
                            case _:
                                return False

                    progress_parsers = [
                        p for p in p.parser.parsers if not ignore_subparser(p)
                    ]
                    check_source_language(
                        len(progress_parsers) == 1,
                        "Top-level information loss prevents unparsers"
                        " generation",
                        location=p.location,
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
        check_source_language(
            bool(self.nodes_to_rules.get(node))
            or node.abstract
            or node.synthetic,
            f"{node.dsl_name} is not synthetic nor abstract, so at least"
            " one parser must create it",
            location=node.location,
        )

    def check_nodes_to_rules(self, ctx: CompileCtx) -> None:
        """
        Check the results of the compute pass, to see if every node type only
        has one non ambiguous way of being unparsed, and assign a canonical
        representation to every node type.

        Combine all unparsers for each node, checking their consistency, and
        attach the result as ``node.unparser``.
        """
        # Check if every non-abstract non-synthetic node has a corresponding
        # parser.
        for node_type in ctx.node_types:
            WarningSet.unused_node_type.warn_if(
                node_type not in self.nodes_to_rules.keys()
                and not node_type.abstract
                and not node_type.synthetic
                and not node_type.is_error_node,
                "{} has no parser, and is marked neither abstract nor"
                " synthetic".format(node_type.dsl_name),
                location=node_type.location,
            )

    def finalize(self, context: CompileCtx) -> None:
        """
        Pass to finalize the preparation of unparsers code generation.
        """
        if not self.context.generate_unparsers:
            return

        assert self.context.lexer
        for rule_assoc in self.context.lexer.rules:
            if isinstance(rule_assoc.action, Ignore):
                error(
                    "Ignore() tokens are incompatible with unparsers. Consider"
                    " using WithTrivia() instead.",
                    location=rule_assoc.action.location,
                )

        # Combine all unparsers for each node, except synthetic/error/abstract
        # nodes. Check that they are consistent. Iterate on all nodes first to
        # get deterministic iteration.
        for node in self.context.node_types:
            if node.abstract or node.synthetic or node.is_error_node:
                continue

            # Make sure we had at least one non-null unparser for every node
            unparsers = [
                u
                for u in self.unparsers[node]
                if not isinstance(u, NullNodeUnparser)
            ]
            check_source_language(
                bool(unparsers),
                "No non-null unparser for non-synthetic node: {}".format(
                    node.dsl_name
                ),
                location=node.location,
            )

            combined = unparsers.pop(0)
            for unparser in unparsers:
                combined = combined.combine(unparser)
                assert type(unparser) == type(
                    combined
                ), "Incompatible unparsers:\n{}\n... and...\n{}".format(
                    combined.dumps(), unparser.dumps()
                )
            node.unparser = combined
            node.unparser.collect(self)

        self.token_sequence_unparsers.finalize()

    def dump(self) -> None:
        """
        Print a debug representation of all node unparsers on the standard
        output.
        """
        for n in self.context.node_types:
            if n.unparser is not None:
                n.unparser.dump()
