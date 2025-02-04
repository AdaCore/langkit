from __future__ import annotations

import dataclasses
from typing import Any, Union, cast

from langkit.compile_context import CompileCtx
from langkit.compiled_types import ASTNodeType, CompiledTypeRepo
from langkit.diagnostics import Location, check_source_language, error
import langkit.expressions as E
from langkit.frontend.annotations import (
    AnnotationSpec,
    FlagAnnotationSpec,
    ParsedAnnotations,
    parse_annotations,
)
from langkit.frontend.scopes import Scope, create_root_scope
from langkit.frontend.static import denoted_str
from langkit.frontend.utils import (
    find_toplevel_decl,
    lkt_context,
    lkt_doc,
    name_from_lower,
)
import langkit.names as names
from langkit.parsers import (
    Cut,
    Defer,
    Discard,
    DontSkip,
    Grammar,
    List as PList,
    ListSepExtra,
    Null,
    Opt,
    Or,
    Parser,
    Pick,
    Predicate,
    Skip,
    StopCut,
    _Row,
    _Token,
    _Transform,
)

import liblktlang as L


class WithLexerAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_lexer annotations for grammar declarations.
    """
    def __init__(self) -> None:
        super().__init__('with_lexer', unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        assert not kwargs
        assert len(args) == 1
        requested_name = args[0]

        # Get the lexer declaration (find_toplevel_decl checks that there is
        # exactly one).
        full_decl = find_toplevel_decl(
            ctx, ctx.lkt_units, L.LexerDecl, "lexer"
        )
        decl = full_decl.f_decl
        assert isinstance(decl, L.LexerDecl)

        # Make sure the name mentionned in this annotation matches the actual
        # lexer name.
        with lkt_context(requested_name):
            check_source_language(
                decl.f_syn_name.text == requested_name.text,
                f"Invalid lexer name: '{decl.f_syn_name.text}' expected",
            )
            return decl


@dataclasses.dataclass
class GrammarAnnotations(ParsedAnnotations):
    with_unparsers: bool
    with_lexer: L.LexerDecl
    annotations = [
        FlagAnnotationSpec("with_unparsers"), WithLexerAnnotationSpec()
    ]


@dataclasses.dataclass
class GrammarRuleAnnotations(ParsedAnnotations):
    main_rule: bool
    entry_point: bool
    annotations = [
        FlagAnnotationSpec('main_rule'),
        FlagAnnotationSpec('entry_point'),
    ]


def create_grammar(ctx: CompileCtx,
                   lkt_units: list[L.AnalysisUnit]) -> Grammar:
    """
    Create a grammar from a set of Lktlang units.

    Note that this only initializes a grammar and fetches relevant declarations
    in the Lktlang unit. The actual lowering on grammar rules happens in a
    separate pass: see lower_all_lkt_rules.

    :param lkt_units: Non-empty list of analysis units where to look for the
        grammar.
    """
    root_scope = create_root_scope(ctx)

    # Look for the GrammarDecl node in top-level lists
    full_grammar = find_toplevel_decl(ctx, lkt_units, L.GrammarDecl, 'grammar')
    assert isinstance(full_grammar.f_decl, L.GrammarDecl)

    # Ensure the grammar name has proper casing
    _ = name_from_lower(ctx, "grammar", full_grammar.f_decl.f_syn_name)

    with lkt_context(full_grammar):
        annotations = parse_annotations(
            ctx, GrammarAnnotations, full_grammar, root_scope
        )

    # Collect the list of grammar rules. This is where we check that we only
    # have grammar rules, that their names are unique, and that they have valid
    # annotations.
    all_rules: list[tuple[str, L.Decl, L.GrammarExpr]] = []
    main_rule_name = None
    entry_points: set[str] = set()
    for full_rule in full_grammar.f_decl.f_rules:
        with lkt_context(full_rule):
            r = full_rule.f_decl

            if not isinstance(r, L.GrammarRuleDecl):
                error(f"grammar rule expected, got {r.p_decl_type_name}")
            rule_name = r.f_syn_name.text

            # Ensure the parsing rule name has proper casing
            _ = name_from_lower(ctx, "parsing rule", r.f_syn_name)

            # Register this rule as a main rule or an entry point if the
            # corresponding annotations are present.
            anns = parse_annotations(
                ctx, GrammarRuleAnnotations, full_rule, root_scope
            )
            if anns.main_rule:
                check_source_language(
                    main_rule_name is None,
                    "only one main rule allowed",
                )
                main_rule_name = rule_name
            if anns.main_rule or anns.entry_point:
                entry_points.add(rule_name)

            all_rules.append((rule_name, r, r.f_expr))

    # Now create the result grammar
    if main_rule_name is None:
        with lkt_context(full_grammar.f_decl):
            error("main rule missing (@main_rule annotation)")
    result = Grammar(
        main_rule_name,
        entry_points,
        annotations.with_unparsers,
        Location.from_lkt_node(full_grammar),
    )

    # Translate rules (all_rules) later, as node types are not available yet
    result._all_lkt_rules += all_rules
    return result


def lower_grammar_rules(ctx: CompileCtx) -> None:
    """
    Translate syntactic L rules to Parser objects.
    """
    lexer = ctx.lexer
    assert lexer is not None

    lexer_tokens = lexer.tokens
    assert lexer_tokens is not None

    grammar = ctx.grammar
    assert grammar is not None

    # Build a mapping for all tokens registered in the lexer. Use camel case
    # names, as this is what the concrete syntax is supposed to use.
    tokens = {cast(names.Name, token.name).camel: token
              for token in lexer_tokens.tokens}

    # Build a mapping for all nodes created in the DSL. We cannot use T (the
    # TypeRepo instance) as types are not processed yet.
    nodes = {n.raw_name.camel: n
             for n in CompiledTypeRepo.astnode_types}

    # For every non-qualifier enum node, build a mapping from value names
    # (camel cased) to the corresponding enum node subclass.
    enum_nodes = {
        node: node._alternatives_map
        for node in nodes.values()
        if node.is_enum_node and not node.is_bool_node
    }

    NodeRefTypes = Union[L.DotExpr, L.TypeRef, L.RefId]

    def resolve_node_ref_or_none(
        node_ref: NodeRefTypes | None
    ) -> ASTNodeType | None:
        """
        Convenience wrapper around resolve_node_ref to handle None values.
        """
        if node_ref is None:
            return None
        return resolve_node_ref(node_ref)

    def resolve_node_ref(node_ref: NodeRefTypes) -> ASTNodeType:
        """
        Helper to resolve a node name to the actual AST node.

        :param node_ref: Node that is the reference to the AST node.
        """
        if isinstance(node_ref, L.DotExpr):
            # Get the altenatives mapping for the prefix_node enum node
            prefix_node = resolve_node_ref(cast(NodeRefTypes,
                                                node_ref.f_prefix))
            with lkt_context(node_ref.f_prefix):
                try:
                    alt_map = enum_nodes[prefix_node]
                except KeyError:
                    error('Non-qualifier enum node expected (got {})'
                          .format(prefix_node.dsl_name))

            # Then resolve the alternative
            suffix = node_ref.f_suffix.text
            with lkt_context(node_ref.f_suffix):
                try:
                    return alt_map[suffix]
                except KeyError:
                    error('Unknown enum node alternative: {}'.format(suffix))

        elif isinstance(node_ref, L.GenericTypeRef):
            with lkt_context(node_ref.f_type_name):
                check_source_language(
                    node_ref.f_type_name.text == u'ASTList',
                    'Bad generic type name: only ASTList is valid in this'
                    ' context'
                )

            params = node_ref.f_params
            with lkt_context(node_ref):
                check_source_language(
                    len(params) == 1,
                    '1 type argument expected, got {}'.format(len(params))
                )
            node_params = [resolve_node_ref(cast(NodeRefTypes, p))
                           for p in params]
            return node_params[0].list

        elif isinstance(node_ref, L.SimpleTypeRef):
            return resolve_node_ref(cast(NodeRefTypes, node_ref.f_type_name))

        else:
            assert isinstance(node_ref, L.RefId)
            with lkt_context(node_ref):
                node_name = node_ref.text
                try:
                    return nodes[node_name]
                except KeyError:
                    error('Unknown node: {}'.format(node_name))

        raise RuntimeError("unreachable code")

    def lower_or_none(
        rule: L.GrammarExpr | L.GrammarExprList | None
    ) -> Parser | None:
        """
        Like ``lower``, but also accept null grammar expressions.
        """
        return None if rule is None else lower(rule)

    def lower(rule: L.GrammarExpr | L.GrammarExprList) -> Parser:
        """
        Helper to lower one parser.

        :param rule: Grammar rule to lower.
        """
        loc = Location.from_lkt_node(rule)
        with lkt_context(rule):
            if isinstance(rule, L.ParseNodeExpr):
                node = resolve_node_ref(cast(NodeRefTypes, rule.f_node_name))

                # Lower the subparsers
                subparsers = [lower(subparser)
                              for subparser in rule.f_sub_exprs]

                # Qualifier nodes are a special case: we produce one subclass
                # or the other depending on whether the subparsers accept the
                # input.
                if node.is_bool_node:
                    return Opt(*subparsers, location=loc).as_bool(node)

                # Likewise for enum nodes
                elif node.base and node.base.is_enum_node:
                    return _Transform(_Row(*subparsers, location=loc),
                                      node,
                                      location=loc)

                # For other nodes, always create the node when the subparsers
                # accept the input.
                else:
                    return _Transform(parser=_Row(*subparsers), typ=node,
                                      location=loc)

            elif isinstance(rule, L.TokenRef):
                token_name = rule.f_token_name.text
                try:
                    val = tokens[token_name]
                except KeyError:
                    with lkt_context(rule.f_token_name):
                        error(f"Unknown token: {token_name}")

                match_text = ''
                if rule.f_expr:
                    # The grammar is supposed to mainain this invariant
                    assert isinstance(rule.f_expr, L.TokenLit)
                    match_text = denoted_str(rule.f_expr)

                return _Token(val=val, match_text=match_text, location=loc)

            elif isinstance(rule, L.TokenLit):
                return _Token(denoted_str(rule), location=loc)

            elif isinstance(rule, L.GrammarList):
                list_cls = (
                    None
                    if isinstance(rule.f_list_type, L.DefaultListTypeRef) else
                    resolve_node_ref(rule.f_list_type)
                )

                # If present, lower the separator specified
                sep = None
                extra: ListSepExtra | None = None
                if rule.f_sep is not None:
                    sep = lower(rule.f_sep.f_token)
                    if rule.f_sep.f_extra is not None:
                        extra_str = rule.f_sep.f_extra.text
                        try:
                            extra = ListSepExtra[extra_str]
                        except KeyError:
                            with lkt_context(rule.f_sep.f_extra):
                                error('invalid separator "extra" specifier')

                return PList(
                    lower(rule.f_expr),
                    sep=sep,
                    empty_valid=rule.f_kind.text == '*',
                    list_cls=list_cls,
                    extra=extra,
                    location=loc,
                )

            elif isinstance(rule, (L.GrammarImplicitPick,
                                   L.GrammarPick)):
                return Pick(*[lower(subparser) for subparser in rule.f_exprs],
                            location=loc)

            elif isinstance(rule, L.GrammarRuleRef):
                assert grammar is not None
                rule_name = rule.f_node_name.text
                return Defer(rule_name,
                             grammar.rule_resolver(rule_name),
                             location=loc)

            elif isinstance(rule, L.GrammarOrExpr):
                return Or(*[lower(subparser)
                            for subparser in rule.f_sub_exprs],
                          location=loc)

            elif isinstance(rule, L.GrammarOpt):
                return Opt(lower(rule.f_expr), location=loc)

            elif isinstance(rule, L.GrammarOptGroup):
                return Opt(*[lower(subparser) for subparser in rule.f_expr],
                           location=loc)

            elif isinstance(rule, L.GrammarOptError):
                return Opt(lower(rule.f_expr), location=loc).error()

            elif isinstance(rule, L.GrammarOptErrorGroup):
                return Opt(*[lower(subparser) for subparser in rule.f_expr],
                           location=loc).error()

            elif isinstance(rule, L.GrammarExprList):
                return Pick(*[lower(subparser) for subparser in rule],
                            location=loc)

            elif isinstance(rule, L.GrammarDiscard):
                return Discard(lower(rule.f_expr), location=loc)

            elif isinstance(rule, L.GrammarNull):
                return Null(resolve_node_ref(rule.f_name), location=loc)

            elif isinstance(rule, L.GrammarSkip):
                return Skip(resolve_node_ref(rule.f_name), location=loc)

            elif isinstance(rule, L.GrammarDontSkip):
                return DontSkip(lower(rule.f_expr),
                                lower(rule.f_dont_skip),
                                location=loc)

            elif isinstance(rule, L.GrammarCut):
                return Cut()

            elif isinstance(rule, L.GrammarStopCut):
                return StopCut(lower(rule.f_expr))

            elif isinstance(rule, L.GrammarPredicate):
                # We expect rule.f_prop_ref to be a reference to a property.
                # Such things have only one possible syntax:
                # NodeName.property_name.
                prop_ref = rule.f_prop_ref
                if (
                    not isinstance(prop_ref, L.DotExpr)
                    or not isinstance(prop_ref.f_prefix, L.RefId)
                ):
                    with lkt_context(prop_ref):
                        error(
                            'reference to a property expected'
                            ' ("Node.property")'
                        )

                # First resolve the reference to the node
                node = resolve_node_ref(prop_ref.f_prefix)

                # Then resolve the reference to the property
                with lkt_context(prop_ref.f_suffix):
                    prop_name = prop_ref.f_suffix.text
                    try:
                        prop = node.get_abstract_node_data_dict()[prop_name]
                    except KeyError:
                        error(f"{node.dsl_name} has no such entity")

                    if not isinstance(prop, E.PropertyDef):
                        error(
                            "reference to a property expected, got a"
                            f" {prop.kind_name}"
                        )

                    # If properties are compiled through the DSL, their
                    # signature is not available at this stage: the
                    # corresponding validity checks are deferred to the
                    # Predicate parser class.
                    return Predicate(lower(rule.f_expr), prop, location=loc)

            else:
                raise NotImplementedError('unhandled parser: {}'.format(rule))

    for name, rule_doc, rule_expr in grammar._all_lkt_rules:
        grammar._add_rule(name, lower(rule_expr), lkt_doc(rule_doc))
