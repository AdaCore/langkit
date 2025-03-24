from __future__ import annotations

import dataclasses
from typing import Any, cast

from langkit.compile_context import CompileCtx
from langkit.compiled_types import ASTNodeType
from langkit.diagnostics import Location, check_source_language, error
from langkit.frontend.annotations import (
    AnnotationSpec,
    FlagAnnotationSpec,
    ParsedAnnotations,
    parse_annotations,
)
from langkit.frontend.resolver import Resolver
from langkit.frontend.scopes import Scope
from langkit.frontend.static import denoted_str
from langkit.frontend.utils import (
    lkt_context,
    lkt_doc,
    name_from_lower,
)
import langkit.names as names
import langkit.parsers as P

import liblktlang as L


class WithLexerAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_lexer annotations for grammar declarations.
    """

    def __init__(self) -> None:
        super().__init__("with_lexer", unique=True, require_args=True)

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
        full_decl = ctx.lkt_resolver.find_toplevel_decl(L.LexerDecl, "lexer")
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
        FlagAnnotationSpec("with_unparsers"),
        WithLexerAnnotationSpec(),
    ]


@dataclasses.dataclass
class GrammarRuleAnnotations(ParsedAnnotations):
    main_rule: bool
    entry_point: bool
    annotations = [
        FlagAnnotationSpec("main_rule"),
        FlagAnnotationSpec("entry_point"),
    ]


def create_grammar(resolver: Resolver) -> P.Grammar:
    """
    Create a grammar from a set of Lktlang units.

    Note that this only initializes a grammar and fetches relevant declarations
    in the Lktlang unit. The actual lowering on grammar rules happens in a
    separate pass: see lower_all_lkt_rules.

    :param lkt_units: Non-empty list of analysis units where to look for the
        grammar.
    """
    ctx = resolver.context

    lexer = ctx.lexer
    assert lexer is not None

    lexer_tokens = lexer.tokens
    assert lexer_tokens is not None

    # Look for the GrammarDecl node in top-level lists
    full_grammar = resolver.find_toplevel_decl(L.GrammarDecl, "grammar")
    assert isinstance(full_grammar.f_decl, L.GrammarDecl)

    # Ensure the grammar name has proper casing
    _ = name_from_lower(ctx, "grammar", full_grammar.f_decl.f_syn_name)

    with lkt_context(full_grammar):
        annotations = parse_annotations(
            ctx, GrammarAnnotations, full_grammar, resolver.root_scope
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
                ctx, GrammarRuleAnnotations, full_rule, resolver.root_scope
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
    grammar = P.Grammar(
        ctx,
        Location.from_lkt_node(full_grammar),
        main_rule_name,
        entry_points,
        annotations.with_unparsers,
    )

    # Build a mapping for all tokens registered in the lexer. Use camel case
    # names, as this is what the concrete syntax is supposed to use.
    tokens = {
        cast(names.Name, token.name).camel: token
        for token in lexer_tokens.tokens
    }

    def resolve_node_ref(ref: L.TypeRef) -> ASTNodeType:
        """
        Helper to resolve a node reference to the corresponding ``ASTNodeType``
        instance.
        """
        return resolver.resolve_node(ref, resolver.root_scope)

    def lower(rule: L.GrammarExpr | L.GrammarExprList) -> P.Parser:
        """
        Helper to lower one parser.

        :param rule: Grammar rule to lower.
        """
        loc = Location.from_lkt_node(rule)
        with lkt_context(rule):
            if isinstance(rule, L.ParseNodeExpr):
                node = resolve_node_ref(rule.f_node_name)

                # Lower the subparsers
                subparsers = [
                    lower(subparser) for subparser in rule.f_sub_exprs
                ]

                # Qualifier nodes are a special case: we produce one subclass
                # or the other depending on whether the subparsers accept the
                # input.
                if node.is_bool_node:
                    return P.Opt(ctx, loc, *subparsers).as_bool(node)

                # Likewise for enum nodes
                elif node.base and node.base.is_enum_node:
                    return P._Transform(
                        ctx, loc, P._Row(ctx, loc, *subparsers), node
                    )

                # For other nodes, always create the node when the subparsers
                # accept the input.
                else:
                    return P._Transform(
                        ctx, loc, P._Row(ctx, loc, *subparsers), typ=node
                    )

            elif isinstance(rule, L.TokenRef):
                token_name = rule.f_token_name.text
                try:
                    val = tokens[token_name]
                except KeyError:
                    with lkt_context(rule.f_token_name):
                        error(f"Unknown token: {token_name}")

                match_text = ""
                if rule.f_expr:
                    # The grammar is supposed to mainain this invariant
                    assert isinstance(rule.f_expr, L.TokenLit)
                    match_text = denoted_str(rule.f_expr)

                return P._Token(
                    context=ctx, location=loc, val=val, match_text=match_text
                )

            elif isinstance(rule, L.TokenLit):
                return P._Token(ctx, loc, denoted_str(rule))

            elif isinstance(rule, L.GrammarList):
                list_cls = (
                    None
                    if isinstance(rule.f_list_type, L.DefaultListTypeRef)
                    else resolve_node_ref(rule.f_list_type)
                )

                # If present, lower the separator specified
                sep = None
                extra: P.ListSepExtra | None = None
                if rule.f_sep is not None:
                    sep = lower(rule.f_sep.f_token)
                    if rule.f_sep.f_extra is not None:
                        extra_str = rule.f_sep.f_extra.text
                        try:
                            extra = P.ListSepExtra[extra_str]
                        except KeyError:
                            with lkt_context(rule.f_sep.f_extra):
                                error('invalid separator "extra" specifier')

                return P.List(
                    ctx,
                    loc,
                    lower(rule.f_expr),
                    sep=sep,
                    empty_valid=rule.f_kind.text == "*",
                    list_cls=list_cls,
                    extra=extra,
                )

            elif isinstance(rule, (L.GrammarImplicitPick, L.GrammarPick)):
                return P.Pick(
                    ctx, loc, *[lower(subparser) for subparser in rule.f_exprs]
                )

            elif isinstance(rule, L.GrammarRuleRef):
                assert grammar is not None
                rule_name = rule.f_node_name.text
                return P.Defer(
                    ctx, loc, rule_name, grammar.rule_resolver(rule_name)
                )

            elif isinstance(rule, L.GrammarOrExpr):
                return P.Or(
                    ctx,
                    loc,
                    *[lower(subparser) for subparser in rule.f_sub_exprs],
                )

            elif isinstance(rule, L.GrammarOpt):
                return P.Opt(ctx, loc, lower(rule.f_expr))

            elif isinstance(rule, L.GrammarOptGroup):
                return P.Opt(
                    ctx, loc, *[lower(subparser) for subparser in rule.f_expr]
                )

            elif isinstance(rule, L.GrammarOptError):
                return P.Opt(ctx, loc, lower(rule.f_expr)).error()

            elif isinstance(rule, L.GrammarOptErrorGroup):
                return P.Opt(
                    ctx, loc, *[lower(subparser) for subparser in rule.f_expr]
                ).error()

            elif isinstance(rule, L.GrammarExprList):
                return P.Pick(
                    ctx, loc, *[lower(subparser) for subparser in rule]
                )

            elif isinstance(rule, L.GrammarDiscard):
                return P.Discard(ctx, loc, lower(rule.f_expr))

            elif isinstance(rule, L.GrammarNull):
                return P.Null(ctx, loc, resolve_node_ref(rule.f_name))

            elif isinstance(rule, L.GrammarSkip):
                return P.Skip(ctx, loc, resolve_node_ref(rule.f_name))

            elif isinstance(rule, L.GrammarDontSkip):
                return P.DontSkip(
                    ctx, loc, lower(rule.f_expr), lower(rule.f_dont_skip)
                )

            elif isinstance(rule, L.GrammarCut):
                return P.Cut(ctx, loc)

            elif isinstance(rule, L.GrammarStopCut):
                return P.StopCut(ctx, loc, lower(rule.f_expr))

            elif isinstance(rule, L.GrammarPredicate):
                return P.Predicate(
                    context=ctx,
                    location=loc,
                    parser=lower(rule.f_expr),
                    property_ref=resolver.resolve_property(rule.f_prop_ref),
                )

            else:
                raise NotImplementedError("unhandled parser: {}".format(rule))

    for name, rule_doc, rule_expr in all_rules:
        grammar.add_rule(name, lower(rule_expr), lkt_doc(rule_doc))

    return grammar
