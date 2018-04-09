from __future__ import absolute_import, division, print_function

"""
Generation of automatic unparsers for Langkit grammars.
"""

from collections import defaultdict

from funcy import split

from langkit.diagnostics import WarningSet
from langkit.parsers import (
    Defer, DontSkip, List, NoBacktrack, Null, Opt, Or, Predicate, Skip,
    _Extract, _Row, _Token, _Transform
)
from langkit.utils import Log, is_same, issubtype


class NodeToParsersPass(object):
    """
    This pass computes the correspondence between AST node types and parsers.
    The end goal is to have one and only one non-ambiguous rule to unparse an
    AST type.
    """

    def __init__(self, context):
        self.context = context
        self.nodes_to_rules = defaultdict(list)

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

        def compute_internal(p):

            # Reject parsing constructs that get in the way of sound unparsers
            if isinstance(p, Or) and not creates_node(p):
                self.abort_unparser('Or() does more that just creating a'
                                    ' node.')

            # Skip parsers create nodes out of thin air in reaction to a
            # parsing error, so unparser logics must ignore them.
            if isinstance(p, Skip):
                pass

            elif isinstance(p, Opt) and p._booleanize:
                for alt in p.get_type()._alternatives:
                    append(alt, p)
                append(p.get_type(), p)

            elif isinstance(p, (List, _Transform)):
                append(p.get_type(), p)

            for c in p.children():
                compute_internal(c)

        if not creates_node(parser):
            self.abort_unparser("'{}' toplevel rule loses information.".format(
                parser.name
            ))
        compute_internal(parser)

    def check_nodes_to_rules(self, ctx):
        """
        Check the results of the compute pass, to see if every node type only
        has one non ambiguous way of being unparsed, and assign a canonical
        representation to every node type.
        """
        from langkit.compiled_types import CompiledTypeMetaclass

        # Check if every non-abstract non-synthetic node has a corresponding
        # parser.
        for node_type in CompiledTypeMetaclass.astnode_types:
            with node_type.diagnostic_context:
                WarningSet.unused_node_type.warn_if(
                    node_type not in self.nodes_to_rules.keys() and
                    not node_type.abstract and
                    not node_type.synthetic and
                    # We don't warn for base list types if they're not used,
                    # because the user has no way to mark them as abstract.
                    not (
                        node_type.is_list_type and
                        node_type.element_type.list == node_type
                    ),
                    '{} has no parser, and is marked neither abstract nor'
                    ' synthetic'.format(node_type.name)
                )

        # Exit early if unparser generation was not requested
        if not ctx.generate_unparser:
            return

        for node, parsers in self.nodes_to_rules.items():
            # Check that all parsers are structurally equivalent, then consider
            # only the canonical one to generate the unparser.
            if not unparser_struct_eq(parsers):
                self.abort_unparser(
                    'Node {} is parsed in different incompatible ways.'.format(
                        node.name
                    )
                )
                return
            node.parser = find_canonical_parser(parsers)
            Log.log('unparser_canonical', node.name, node.parser)


def creates_node(p):
    """
    Return true on parsers that create a node directly, or are just a reference
    to one or several parsers that creates nodes, without additional parsing
    involved.

    For example::
        Node(..)               # <- True
        Or(a, b, c)            # <- True if a b & c creates_node
        _Row(a, b, c)          # <- False
        Pick(";", "lol", c)    # <- False

    :param Parser p: Parser to analyze.
    """
    from langkit.dsl import EnumNode
    from langkit.lexer import LexerToken

    if isinstance(p, Or):
        return all(creates_node(c) for c in p.children())

    if isinstance(p, Defer):
        return p.get_type().is_ast_node

    if isinstance(p, Opt) and creates_node(p.parser):
        return True

    if isinstance(p, Predicate):
        return creates_node(p.parser)

    # As a special case, if "p" parses a node followed by a termination token,
    # then consider it just creates a node.
    if isinstance(p, _Extract):
        if len(p.parser.parsers) != 2:
            return False
        node, term = p.parser.parsers
        return (creates_node(node) and
                isinstance(term, _Token) and
                term._val == LexerToken.Termination)

    return (
        isinstance(p, _Transform)
        or isinstance(p, Skip)
        or isinstance(p, List)
        or (isinstance(p, Opt) and issubtype(p._booleanize, EnumNode))
    )


@Log.recursive
@Log.log_return('unparser_eq_impl')
def unparser_struct_eq(parsers, toplevel=True):
    """
    Determine if all given parsers are structurally equal with regards to
    unparsing.

    :param list[Parser] parsers: List of parsers to compare. Must contain at
        least one parser.
    :param bool toplevel: Recursion helper.
    :rtype: bool
    """
    def unwrap(p):
        return p.subparser if isinstance(p, DontSkip) else p

    parsers = [unwrap(p) for p in parsers if not isinstance(p, Null)]

    Log.log('unparser_eq_impl', 'parsers: {}'.format(parsers))

    # If there is only one parser, the result is obviously True
    if len(parsers) == 1:
        return True

    parsers_types = set(type(p) for p in parsers)

    # If all parsers are of the same kind, let's see if they're structurally
    # equivalent.
    if len(parsers_types) == 1:
        # "typ" is the only parser kind we have in "parsers"
        typ = parsers_types.pop()

        # For those parser kinds, we only need to check that their lists of
        # children are equivalent.
        if typ in (_Row, _Transform, List, Opt):

            # We skip NoBacktrack parsers in structural comparison because they
            # have no effect on unparsing.
            children_lists = [[subp for subp in p.children()
                               if not isinstance(subp, NoBacktrack)]
                              for p in parsers]

            return is_same(len(c) for c in children_lists) and all(
                unparser_struct_eq(c, False)
                for c in zip(*children_lists)
            )

        # For Tok, we want to check that the parsed token is the same
        elif typ == _Token:
            return is_same(p.val for p in parsers)

        # For _Extract, structural equality involves comparing the sub-parser
        # and the extracted index.
        elif typ == _Extract:
            return (unparser_struct_eq(p.parser for p in parsers)
                    and is_same(p.index for p in parsers))

        # Defer and Or will be handled by the same logic we use when the kind
        # of parser is not unique.
        elif typ in (Defer, Or):
            pass
        else:
            raise NotImplementedError('Parser type not handled')

    # If we fall down here, either:
    # 1. There are more than one parser kind.
    # 2. The kind is one of those not handled by the block of code above (Or
    #    and Defer).

    # We will use a specific logic for sub-parsers (toplevel=False): if they
    # all create nodes directly, without adding additional parser logic, then
    # their uniqueness is already checked because we call unparser_struct_eq on
    # all of those.
    if not toplevel:
        resolved_parsers = [p.parser if isinstance(p, Defer) else p
                            for p in parsers]
        return all(creates_node(p) for p in resolved_parsers)

    return False


def find_canonical_parser(parsers):
    """
    From a list of parsers corresponding to the same node type, return the one
    that will be used to emit the unparser, which is considered the canonical
    one for unparsing.

    :param list[parsers] parsers: List of parsers to analyze.
    :rtype: Parser
    """
    def has_null(parser):
        """
        Return whether `parser` is a Null or recursivery has a Null children
        parser.
        """
        return isinstance(parser, Null) or any(has_null(c)
                                               for c in parser.children())

    nulls, no_nulls = split(has_null, parsers)
    return no_nulls[0] if no_nulls else nulls[0]
