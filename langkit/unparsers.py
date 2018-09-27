from __future__ import absolute_import, division, print_function

"""
Generation of automatic unparsers for Langkit grammars.
"""

from collections import defaultdict
import itertools
from StringIO import StringIO
import sys

from langkit.common import string_repr
from langkit.compiled_types import get_context
from langkit.diagnostics import WarningSet, check_source_language
from langkit.lexer import Ignore, LexerToken
import langkit.names as names
from langkit.parsers import (
    Defer, DontSkip, List, NoBacktrack, Null, Opt, Or, Skip, _Extract, _Row,
    _Token, _Transform, Predicate
)
from langkit.utils import not_implemented_error


def unwrap(parser):
    """
    Strip DontSkip and Predicate parser layers.

    :type parser: Parser
    :rtype: Parser
    """
    while True:
        if isinstance(parser, DontSkip):
            parser = parser.subparser
        elif isinstance(parser, Predicate):
            parser = parser.parser
        else:
            return parser


class Unparser(object):
    """
    Abstract class for unparsers.
    """

    def dump(self, stream=None):
        """
        Print a debug representation of this unparser to the ``stream`` file.

        :param file|None stream: Stream to emit debug representation to.
            ``sys.stdout`` is used if left to None.
        """
        stream = stream or sys.stdout
        self._dump(stream)

    def dumps(self):
        """
        Return a debug representation of this unparser.

        :rtype: str
        """
        result = StringIO()
        self.dump(result)
        return result.getvalue()

    def _dump(self, stream):
        """
        Concrete subclasses must override this to implement the dump method.
        """
        raise not_implemented_error(self, type(self)._dump)

    def combine(self, other):
        """
        Return an unparser that combines `self` and `other`.

        Both are assumed to be of the same type. If both contain contradictory
        information, emit a user error.

        :param Unparser other: Unparser to combine with ``self``. Must be an
            other instance of self's type.
        :rtype: Unparser
        """
        assert isinstance(other, type(self)), (
            'Incompatible unparsers:\n{}\n... and...\n{}'.format(
                self.dumps(), other.dumps()
            )
        )
        return self._combine(other)

    def _combine(self, other):
        """
        Concrete subclasses must override this to implement the combine method.
        """
        raise not_implemented_error(self, type(self)._combine)

    def collect(self, unparsers):
        """
        Traverse all unparsers in ``self`` (in prefix order) and register the
        various unparsers that need to be collected in ``unparsers``. This
        de-duplicates token sequence unparsers.

        :param Unparsers unparsers: Collection of unparsers to complete.
        """
        raise not_implemented_error(self, type(self).collect)


class TokenUnparser(Unparser):
    """
    Unparser for a token. The token text must be known.
    """

    # Internal counter to generate unique variable names
    _counter = itertools.count(0)

    def __init__(self, token, match_text=None):
        """
        :param langkit.lexer.TokenAction token: Kind for the token to unparse.
        :param str|None match_text: If there is no literal corresponding to
            ``token``, this must be a string used for unparsing. Otherwise,
            this must be None.
        """
        assert (token.matcher is None) == bool(match_text)
        self.token = token
        self.match_text = match_text

        self._var_name = None

    @classmethod
    def from_parser(cls, parser):
        """
        Create a token unparser out of a parser, assumed to parse a token.
        If ``parser`` is None, return None.

        :param _Token|None parser: Token parser to analyze.
        :rtype: TokenUnparser|None
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
    def equivalent(token1, token2):
        """
        Return whether `token1` and `token2` are equivalent tokens.

        :type TokenUnparser: token1
        :type TokenUnparser: token2
        :rtype: bool
        """
        if token1 is None or token2 is None:
            return token1 is token2
        return token1.dumps() == token2.dumps()

    @staticmethod
    def dump_or_none(token):
        """
        Return ``token.dumps()`` unless it is None.

        :rtype: str
        """
        return '<none>' if token is None else token.dumps()

    def _dump(self, stream):
        stream.write(self.match_text
                     if self.match_text else
                     self.token.matcher.to_match)

    @property
    def string_repr(self):
        """
        Return an Ada string literal for the text to emit this token.

        :rtype: str
        """
        return string_repr(self.dumps())

    # Comparing tokens is done through
    # ``TokenSequenceUnparser.check_equivalence``, which is already good at
    # providing context for users in diagnostics, so deliberately not
    # overriding the "_combine" method.

    def __repr__(self):
        return 'Token {}'.format(repr(self.dumps()))

    @property
    def var_name(self):
        """
        Name of the variable to hold this sequence of tokens in code
        generation.

        :rtype: names.Name
        """
        if self._var_name is None:
            self._var_name = names.Name('Token_Unparser_{}'
                                        .format(next(self._counter)))
        return self._var_name


class TokenSequenceUnparser(Unparser):
    """
    Sequence of token unparsers.
    """

    def __init__(self, init_tokens=None):
        """
        :param TokenSequenceUnparser|None init_tokens: Optional list of tokens
            to start with.
        """
        self.tokens = list(init_tokens or [])

        self._serial_number = None
        self._var_name = None

    def _dump(self, stream):
        if self.tokens:
            stream.write(' '.join(t.dumps() for t in self.tokens))
        else:
            stream.write('<no token>')

    def __len__(self):
        return len(self.tokens)

    def __add__(self, other):
        """
        Return a new token sequence unparser that is the concatenation of
        ``self`` and ``other``.

        :type other: TokenSequenceUnparser
        :rtype: TokenSequenceUnparser
        """
        return TokenSequenceUnparser(self.tokens + other.tokens)

    def append(self, token):
        """
        Append a token to this sequence.

        :param TokenUnparser token: Token unparser to append.
        """
        self.tokens.append(token)

    def check_equivalence(self, sequence_name, other):
        """
        Emit a user diagnostic if `self` and `other` are not equivalent
        sequences of token unparsers.

        :param str sequence_name: Name of the token sequences to compare, used
            in the diagnostic label.
        :param TokenSequenceUnparser other: Sequence to compare to ``self``.
        """
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
    def var_name(self):
        """
        Name of the variable to hold this sequence of tokens in code
        generation.

        :rtype: names.Name
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

    @staticmethod
    def from_parser(node, parser):
        """
        Given a parser that creates a specific type of parse node, return the
        corresponding unparser. Emit a user diagnostic if this transformation
        cannot be made.

        :param ASTNodeType node: Parse node that `parser` emits.
        :param Parser parser: Parser for which we want to create an unparser.
        :rtype: NodeUnparser
        """
        assert not node.abstract and not node.synthetic, (
            'Invalid unparser request for {}'.format(node.dsl_name)
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
                    TokenUnparser.from_parser(parser.sep)
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
                    if node is parser._booleanize._alt_present.type:
                        NodeUnparser._emit_to_token_sequence(parser.parser,
                                                             result.pre_tokens)
                    return result

                else:
                    return NodeUnparser.from_parser(node, parser.parser)

            if isinstance(parser, Null):
                return NullNodeUnparser(node)

            check_source_language(
                False,
                'Unsupported parser for unparsers generation: {}'.format(
                    parser
                )
            )

    @staticmethod
    def _from_transform_parser(node, parser):
        """
        Helper for _from_parser. Turn the given _Transform parser into a
        RegularNodeUnparser instance.

        :param ASTNodeType node: Parse node that `parser` emits.
        :param Parser parser: Parser for which we want to create an unparser.
        :rtype: NodeUnparser
        """
        assert isinstance(parser, _Transform)
        assert isinstance(parser.parser, _Row)

        result = RegularNodeUnparser(node)
        subparsers = parser.parser.parsers
        next_field = 0

        def surrounding_inter_tokens():
            """
            Considering the next field to process, return a tuple that contains
            the token sequence that precedes it, and the token sequence that
            succeeds it.

            :rtype: (TokenSequenceUnparser, TokenSequenceUnparser|None)
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
                NodeUnparser._emit_to_field_unparser(
                    subp, result.field_unparsers[next_field],
                    pre_tokens, post_tokens
                )
                next_field += 1

        return result

    @staticmethod
    def _from_token_node_parser(node, parser):
        """
        Helper for _from_parser. Assuming ``node`` is a token node, turn the
        given ``parser`` into a TokenNodeUnparser instance.

        :param ASTNodeType node: Parse node that `parser` emits.
        :param Parser parser: Parser for which we want to create an unparser.
        :rtype: RegularNodeUnparser
        """
        assert node.is_token_node

        accepted = True
        if isinstance(parser, _Transform):
            # All _Transform parsers contain a _Row subparser
            assert isinstance(parser.parser, _Row)

            # Previous validation passes ensure that parsers for token nodes
            # parse exactly one token, so the assertion below should stand.
            subparsers = parser.parser.parsers
            assert len(subparsers) == 1

            token_parser = subparsers[0]
            accepted = isinstance(token_parser, _Token)

            if accepted:
                token_kind = token_parser.val
                if node.token_kind not in (None, token_kind):
                    check_source_language(
                        False,
                        'The {} token node can be associated to only one token'
                        ' kind: here we have {}, but we already had {}'.format(
                            node.dsl_name, token_kind.dsl_name,
                            node.token_kind.dsl_name
                        )
                    )
                node.token_kind = token_kind

        else:
            accepted = False

        check_source_language(
            accepted,
            'Unsupported token node parser for unparsers generation, only'
            ' direct token parsers are accepted: {}'.format(parser)
        )
        return TokenNodeUnparser(node)

    @staticmethod
    def _split_extract(parser):
        """
        Split ``_Extract`` parsers into three parts: a sequence of pre-tokens,
        the node parser in the middle, and a sequence of post-tokens.

        :param _Extract parser: _Extract parser to split.
        :rtype: (TokenSequenceUnparser, Parser, TokenSequenceUnparser)
        """
        assert isinstance(parser, _Extract)
        assert isinstance(parser.parser, _Row)
        subparsers = parser.parser.parsers
        index = parser.index

        pre_toks = []
        for pre_parser in subparsers[:index]:
            NodeUnparser._emit_to_token_sequence(pre_parser, pre_toks)

        node_parser = subparsers[parser.index]

        post_toks = []
        for post_parser in subparsers[index + 1:]:
            NodeUnparser._emit_to_token_sequence(post_parser, post_toks)

        return (TokenSequenceUnparser(pre_toks),
                node_parser,
                TokenSequenceUnparser(post_toks))

    @staticmethod
    def _emit_to_token_sequence(parser, token_sequence):
        """
        Turn the given parser into a sequence of tokens.

        Emit a user diagnostic if ``parser`` is a parser that does not parse
        exactly a constant sequence of tokens.

        :param Parser parser: Parser to analyze.
        :param TokenSequenceUnparser token_sequence: List into which this
            appends the sequence of tokens.
        """
        parser = unwrap(parser)

        if isinstance(parser, _Row):
            for subparser in parser.parsers:
                NodeUnparser._emit_to_token_sequence(subparser, token_sequence)

        elif isinstance(parser, _Token):
            token_sequence.append(TokenUnparser.from_parser(parser))

        elif isinstance(parser, Opt) and parser._is_error:
            NodeUnparser._emit_to_token_sequence(parser.parser, token_sequence)

        elif isinstance(parser, (DontSkip, NoBacktrack)):
            pass

        else:
            check_source_language(
                False,
                'Static sequence of tokens expected, but got: {}'.format(
                    parser
                )
            )

    @staticmethod
    def _emit_to_field_unparser(parser, field_unparser, pre_tokens,
                                post_tokens):
        """
        Considering ``field_unparser`` as a field unparser we are in the
        process of elaborating, and ``pre_tokens`` and ``post_tokens`` as the
        token sequences that surround this field, extract information from the
        given ``parser`` to complete them.

        If ``parser`` is anything else than a Null parser, set
        ``field_unparser.always_absent`` to True.

        Emit a user diagnostic if ``parser`` is too complex for this analysis.

        :param Parser parser: Parser to analyze.
        :param FieldUnparser field_unparser: Field unparser to complete.
        :param TokenSequenceUnparser pre_tokens: Token sequences to contain the
            list of tokens that appear before the field, whether or not the
            field is present. Tokens are inserted at the end of this sequence.
        :param TokenSequenceUnparser post_tokens: Token sequences to contain
            the list of tokens that appear after the field, whether or not the
            field is present. Tokens are inserted at the beginning of this
            sequence.
        """
        parser = unwrap(parser)

        # As all fields are nodes, previous validation passes made sure that
        # `parser` yields a parse node (potentially a null one).

        if isinstance(parser, (Defer, List, Null, _Transform)):
            # Field parsing goes directly to node creation, so there is no
            # pre/post sequences of tokens.
            field_unparser.always_absent = (field_unparser.always_absent and
                                            isinstance(parser, Null))

        elif isinstance(parser, Opt):
            if not parser._booleanize:
                # Because we are in an Opt parser, we now know that this field
                # is optionnal, so it can be absent.
                field_unparser.always_absent = False
                field_unparser.empty_list_is_absent = (parser.get_type()
                                                       .is_list_type)

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
                if not isinstance(subparser, Defer):
                    NodeUnparser.from_parser(subparser.get_type(), subparser)

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

    def __init__(self, node):
        self.node = node

    def _dump(self, stream):
        stream.write('Unparser for {}: null\n'.format(self.node.dsl_name))

    # Null unparsers are not supposed to be combined with others, so
    # deliberately not overriding the "_combine" method.


class FieldUnparser(Unparser):
    """
    Unparser for a node field.
    """

    def __init__(self, node, field):
        """
        :param ASTNodeType node: The node for which we create this field
            unparser. Because of node inheritance, this can be different than
            `field.struct`.
        :param Field field: Parse field that this unparser handles.
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

    def _dump(self, stream):
        if self.empty_list_is_absent:
            stream.write('   [empty_list_is_absent]\n')
        stream.write('   if {}: {} [field] {}\n'.format(
            self.field.qualname,
            self.pre_tokens.dumps(),
            self.post_tokens.dumps(),
        ))

    def _combine(self, other):
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

    def collect(self, unparsers):
        tok_seq_pool = unparsers.token_sequence_unparsers
        self.pre_tokens = tok_seq_pool.get_unique(self.pre_tokens)
        self.post_tokens = tok_seq_pool.get_unique(self.post_tokens)


class RegularNodeUnparser(NodeUnparser):
    """
    Unparser for "regular" nodes.

    In this context, "regular" means that this node can hahve fields: it's not
    a list and it's not a token node.
    """

    def __init__(self, node):
        """
        :param ASTNodeType node: Parse node that this unparser handles.
        """
        self.node = node

        parse_fields = self.node.get_parse_fields(
            predicate=lambda f: not f.null)

        self.pre_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that precedes this field during (un)parsing.

        :type: TokenSequenceUnparser
        """

        self.field_unparsers = [FieldUnparser(node, field)
                                for field in parse_fields]
        """
        List of field unparsers corresponding to this node's parse fields.

        :type: list[FieldUnparser]
        """

        self.inter_tokens = [TokenSequenceUnparser()
                             for _ in range(len(parse_fields) - 1)]
        """
        List of token sequences, corresponding to tokens that appear between
        parse fields. Token sequence at index N materializes tokes that appear
        between between fields N-1 and N.

        :type: list[TokenSequenceUnparser]
        """

        self.post_tokens = TokenSequenceUnparser()
        """
        Sequence of tokens that follows this field during (un)parsing.

        :type: TokenSequenceUnparser
        """

    @property
    def fields_unparser_var_name(self):
        """
        Return the name of the variable in code generation to store the
        unparsing table for fields.
        """
        return (self.node.name + names.Name('Fields_Unparser_List')
                if self.field_unparsers else
                names.Name('Empty_Field_Unparser_List'))

    @property
    def zip_fields(self):
        """
        Zipped list of field unparsers and inter-field token sequences.

        :rtype: list[(FieldUnparser, TokenSequenceUnparser)]
        """
        return zip(self.field_unparsers,
                   [TokenSequenceUnparser()] + self.inter_tokens)

    def _dump(self, stream):
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

    def _combine(self, other):
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

        result = RegularNodeUnparser(self.node)
        result.pre_tokens = self.pre_tokens
        result.post_tokens = self.post_tokens
        result.inter_tokens = self.inter_tokens
        result.field_unparsers = [
            self_fu.combine(other_fu)
            for self_fu, other_fu in zip(self.field_unparsers,
                                         other.field_unparsers)
        ]
        return result

    def collect(self, unparsers):
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

    def __init__(self, node, separator):
        """
        :param ASTNodeType node: Parse node that this unparser handles.
        :param TokenUnparser|None separator: Unparser for the separator token,
            or None if this list allows no separator.
        """
        self.node = node
        self.separator = separator

    def _dump(self, stream):
        stream.write('Unparser for {}:\n'.format(self.node.dsl_name))
        if self.separator:
            stream.write('   separator: {}\n'.format(self.separator.dumps()))

    def _combine(self, other):
        assert self.node == other.node
        check_source_language(
            TokenUnparser.equivalent(self.separator, other.separator),
            'Inconsistent separation token for {}: {} and {}'.format(
                self.node.dsl_name,
                TokenUnparser.dump_or_none(self.separator),
                TokenUnparser.dump_or_none(other.separator)
            )
        )
        return self

    def collect(self, unparsers):
        pass


class TokenNodeUnparser(NodeUnparser):
    """
    Unparser for token nodes.
    """

    def __init__(self, node):
        """
        :param ASTNodeType node: Parse node that this unparser handles.
        """
        self.node = node

    def _dump(self, stream):
        stream.write('Unparser for {}\n'.format(self.node.dsl_name))

    def _combine(self, other):
        assert self.node == other.node
        return self

    def collect(self, unparsers):
        pass


class TokenSequenceUnparserPool(object):
    """
    Helper to help removing redundant token sequence unparsers.
    """

    def __init__(self):
        self.pool = {}
        """
        :type: dict[list[TokenUnparser], TokenSequenceUnparser]
        """

        self.sorted = None

    def __len__(self):
        return len(self.pool)

    def __getitem__(self, index):
        return self.sorted[index]

    def get_unique(self, token_seq):
        assert self.sorted is None
        key = tuple(token_seq.tokens)
        try:
            return self.pool[key]
        except KeyError:
            self.pool[key] = token_seq
            return token_seq

    def finalize(self):
        self.sorted = sorted(
            self.pool.values(),
            key=lambda tok_seq: tuple(t.string_repr for t in tok_seq.tokens),
        )

        # Assign a unique identification number to token sequences for code
        # generation.
        for i, tok_seq in enumerate(self.sorted):
            tok_seq._serial_number = i


class Unparsers(object):
    """
    Holder for the creation of unparsing tables.

    The end goal is to have one and only one non-ambiguous rule to unparse an
    AST type.
    """

    def __init__(self, context):
        self.context = context

        self.nodes_to_rules = defaultdict(list)

        self.unparsers = defaultdict(list)
        """
        Map instead each node to the corresponding list of unparsers.
        Unparsers are built from parsers that create these nodes.

        :type: dict[ASTNodeType, list[NodeUnparser]]
        """

        self.token_unparsers = {}
        """
        Cache for created token unparsers. This avoids the emission of the same
        token unparser constant over and over in generated code.

        :type: dict[(langkit.lexer.TokenAction, str|None), TokenUnparser]
        """

        self.token_sequence_unparsers = TokenSequenceUnparserPool()
        """
        Pool of all token sequence unparsers in the unparsing tables. Computed
        at the end of finalization to retain only the final ones
        (de-duplicated).

        :type: TokenSequenceUnparserPool
        """

    @property
    def sorted_token_unparsers(self):
        """
        List of all token unparsers. Order is consistent across runs.
        """
        return sorted(self.token_unparsers.values(),
                      key=lambda t: t.dumps())

    def abort_unparser(self, message):
        """
        Abort unparsers generation. Emit a warning to inform users with the
        given message.
        """
        extra_info = (
            '\nFor more information, enable the the unparser_eq trace.'
            if self.context.generate_unparser else ''
        )
        WarningSet.unparser_bad_grammar.warn_if(
            True,
            '{} This prevents the generation of an automatic unparser.{}'
            .format(message, extra_info)
        )
        self.context.generate_unparser = False

    def compute(self, parser):
        """
        Map every AST node type to the set of parsers that return this type.

        If unparsers are requested, compute unparsers for all node-constructing
        sub-parsers in ``parser``.

        Also abort the generation of unparsers if the grammar contain
        parsing constructs we don't support with unparsers.

        :param Parser parser: Parser combinator to analyze.
        """

        # Skip parsers generated for DontSkip. They don't generate any nodes,
        # so are not interesting in that context.
        if parser.is_dont_skip_parser:
            return

        def append(node, parser):
            self.nodes_to_rules[node].append(parser)
            if self.context.generate_unparser:
                self.unparsers[node].append(
                    NodeUnparser.from_parser(node, parser)
                )

        def compute_internal(p, toplevel=True):
            # Skip parsers create nodes out of thin air in reaction to a
            # parsing error, so unparser logics must ignore them.
            if isinstance(p, Skip):
                pass

            elif isinstance(p, Opt) and p._booleanize:
                for alt in p._booleanize._alternatives:
                    append(alt.type, p)
                toplevel = False

            elif isinstance(p, (List, _Transform)):
                append(p.get_type(), p)
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

    def check_nodes_to_rules(self, ctx):
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
                    node_type not in self.nodes_to_rules.keys() and
                    not node_type.abstract and
                    not node_type.synthetic,
                    '{} has no parser, and is marked neither abstract nor'
                    ' synthetic'.format(node_type.name)
                )

    def finalize(self, context):
        """
        Pass to finalize the preparation of unparsers code generation.
        """
        if self.context.generate_unparser:
            ignore_tokens = []
            for rule_assoc in self.context.lexer.rules:
                if isinstance(rule_assoc.action, Ignore):
                    ignore_tokens.append(rule_assoc.action)
            check_source_language(
                not ignore_tokens,
                'Ignore() tokens are incompatible with unparsers.'
                ' Consider using WithTrivia() instead.'
            )

        # Combine all unparsers for each node, checking that they are
        # consistent. Iterate on all nodes first to get deterministic
        # iteration.
        for node in self.context.astnode_types:
            unparsers = self.unparsers[node]
            if not unparsers:
                continue

            # TODO: previous validation passes are supposed to ensure the
            # assertion that follows, right?
            unparsers = [u for u in unparsers
                         if not isinstance(u, NullNodeUnparser)]
            assert unparsers, ('No non-null unparser for non-synthetic node:'
                               ' {}'.format(node.dsl_name))

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
