from __future__ import annotations

import dataclasses
from typing import Any, Union, cast

from langkit.compile_context import CompileCtx
from langkit.diagnostics import Location, check_source_language, error
from langkit.frontend.annotations import (
    AnnotationSpec,
    FlagAnnotationSpec,
    ParsedAnnotations,
    check_no_annotations,
    parse_annotations,
)
from langkit.frontend.scopes import Scope, create_root_scope
from langkit.frontend.static import (
    denoted_str,
    parse_static_bool,
    parse_static_pattern,
)
from langkit.frontend.utils import (
    find_toplevel_decl,
    lkt_context,
    name_from_camel,
    name_from_lower,
)
from langkit.lexer import (
    Action,
    Alt,
    Case,
    Ignore,
    Lexer,
    LexerToken,
    Literal,
    Matcher,
    NoCaseLit,
    Pattern,
    RuleAssoc,
    TokenAction,
    TokenFamily,
    WithSymbol,
    WithText,
    WithTrivia,
)
import langkit.names as names

import liblktlang as L


class TokenAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @text/symbol/trivia annotations for tokens.
    """
    def __init__(self, name: str):
        super().__init__(name, unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        check_source_language(not args, 'No positional argument allowed')
        result: dict[str, Any] = {}

        try:
            expr = kwargs.pop('start_ignore_layout')
        except KeyError:
            result["start_ignore_layout"] = False
        else:
            result["start_ignore_layout"] = parse_static_bool(ctx, expr)

        try:
            expr = kwargs.pop('end_ignore_layout')
        except KeyError:
            result["end_ignore_layout"] = False
        else:
            result["end_ignore_layout"] = parse_static_bool(ctx, expr)

        # The "comment" argument is valid for trivia tokens only
        if self.name == "trivia":
            try:
                expr = kwargs.pop("comment")
            except KeyError:
                result["comment"] = False
            else:
                result["comment"] = parse_static_bool(ctx, expr)

        check_source_language(
            not kwargs,
            'Invalid arguments: {}'.format(', '.join(sorted(kwargs)))
        )

        return result


class SpacingAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @unparsing_spacing annotations for token families.
    """
    def __init__(self) -> None:
        super().__init__('unparsing_spacing', unique=False, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: list[L.Expr],
        kwargs: dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        check_source_language(not args, 'No positional argument allowed')

        try:
            expr = kwargs.pop('with')
        except KeyError:
            error('Missing "with" argument')
        else:
            check_source_language(
                not kwargs,
                'Invalid arguments: {}'.format(', '.join(sorted(kwargs)))
            )
            if not isinstance(expr, L.RefId):
                error('Token family name expected')
            return expr


@dataclasses.dataclass
class LexerAnnotations(ParsedAnnotations):
    case_insensitive: bool
    indentation_tracking: bool
    annotations = [
        FlagAnnotationSpec('case_insensitive'),
        FlagAnnotationSpec('indentation_tracking'),
    ]


@dataclasses.dataclass
class TokenAnnotations(ParsedAnnotations):
    text: tuple[bool, bool]
    trivia: tuple[bool, bool]
    symbol: tuple[bool, bool]
    with_unparsing_newline: bool
    pre_rule: bool
    ignored: bool
    annotations = [
        TokenAnnotationSpec('text'),
        TokenAnnotationSpec('trivia'),
        TokenAnnotationSpec('symbol'),
        FlagAnnotationSpec('with_unparsing_newline'),
        FlagAnnotationSpec('pre_rule'),
        FlagAnnotationSpec('ignored'),
    ]


@dataclasses.dataclass
class TokenFamilyAnnotations(ParsedAnnotations):
    unparsing_spacing: list[L.RefId]
    annotations = [SpacingAnnotationSpec()]


def create_lexer(ctx: CompileCtx, lkt_units: list[L.AnalysisUnit]) -> Lexer:
    """
    Create and populate a lexer from a Lktlang unit.

    :param lkt_units: Non-empty list of analysis units where to look for the
        grammar.
    """
    root_scope = create_root_scope(ctx)

    # Look for the LexerDecl node in top-level lists
    full_lexer = find_toplevel_decl(ctx, lkt_units, L.LexerDecl, 'lexer')
    assert isinstance(full_lexer.f_decl, L.LexerDecl)

    # Ensure the lexer name has proper casing
    _ = name_from_lower(ctx, "lexer", full_lexer.f_decl.f_syn_name)

    with lkt_context(full_lexer):
        lexer_annot = parse_annotations(
            ctx, LexerAnnotations, full_lexer, root_scope
        )

    patterns: dict[names.Name, tuple[str, Location]] = {}
    """
    Mapping from pattern names to the corresponding regular expression.
    """

    token_family_sets: dict[names.Name, tuple[set[TokenAction], Location]] = {}
    """
    Mapping from token family names to the corresponding sets of tokens that
    belong to this family, and the location for the token family declaration.
    """

    token_families: dict[names.Name, TokenFamily] = {}
    """
    Mapping from token family names to the corresponding token families.  We
    build this late, once we know all tokens and all families.
    """

    spacings: list[tuple[names.Name, L.RefId]] = []
    """
    Couple of names for token family between which unparsing must insert
    spaces. The first name is known to be valid, but not the second one, so we
    keep it as a node to create a diagnostic context.
    """

    tokens: dict[names.Name, Action] = {}
    """
    Mapping from token names to the corresponding tokens.
    """

    rules: list[RuleAssoc | tuple[Matcher, Action]] = []
    pre_rules: list[tuple[Matcher, Action]] = []
    """
    Lists of regular and pre lexing rules for this lexer.
    """

    newline_after: list[TokenAction] = []
    """
    List of tokens after which we must introduce a newline during unparsing.
    """

    @dataclasses.dataclass
    class RegularRule:
        token: Action
        is_pre: bool
        matcher_expr: L.GrammarExpr

    SrcRule = Union[RegularRule, L.LexerCaseRule]

    def process_family(f: L.LexerFamilyDecl, rules: list[SrcRule]) -> None:
        """
        Process a LexerFamilyDecl node. Register the token family, the token
        declarations it contains, and append the rules it contains to
        ``rules``.
        """
        with lkt_context(f):
            # Create the token family, if needed
            name = name_from_lower(ctx, "token family", f.f_syn_name)
            token_set, _ = token_family_sets.setdefault(
                name,
                (set(), Location.from_lkt_node(f)),
            )

            for r in f.f_rules:
                if not isinstance(r.f_decl, L.GrammarRuleDecl):
                    error('Only lexer rules allowed in family blocks')
                process_token_rule(r, rules, token_set)

            family_annotations = parse_annotations(
                ctx,
                TokenFamilyAnnotations,
                cast(L.FullDecl, f.parent),
                root_scope,
            )

            for spacing in family_annotations.unparsing_spacing:
                spacings.append((name, spacing))

    def process_token_rule(
        r: L.FullDecl,
        rules: list[SrcRule],
        token_set: set[TokenAction] | None = None,
    ) -> None:
        """
        Process the full declaration of a GrammarRuleDecl node: create the
        token it declares and lower the optional associated lexing rule.

        :param r: Full declaration for the GrammarRuleDecl to process.
        :param rules: List of lexing rules, to be completed with ``r``.
        :param token_set: If this declaration appears in the context of a token
            family, this adds the new token to this set.  Must be left to None
            otherwise.
        """
        with lkt_context(r):
            rule_annot: TokenAnnotations = parse_annotations(
                ctx, TokenAnnotations, r, root_scope
            )

            # Gather token action info from the annotations. If absent,
            # fallback to WithText.
            token_kind: str | None = None
            start_ignore_layout = False
            end_ignore_layout = False
            comment: bool = False
            location = Location.from_lkt_node(r)
            if rule_annot.ignored:
                token_kind = "ignored"
            for name in ('text', 'trivia', 'symbol'):
                annot = getattr(rule_annot, name)
                if not annot:
                    continue
                if token_kind is not None:
                    error('At most one token action allowed')

                token_kind = name
                start_ignore_layout = annot["start_ignore_layout"]
                end_ignore_layout = annot["end_ignore_layout"]
                if "comment" in annot:
                    comment = annot["comment"]

            is_pre = rule_annot.pre_rule
            if token_kind is None:
                token_kind = "text"

            # Create the token and register it where needed: the global token
            # mapping, its token family (if any) and the "newline_after" group
            # if the corresponding annotation is present.
            token_camel_name = r.f_decl.f_syn_name.text
            token_name = (
                None
                if token_camel_name == "_"
                else name_from_camel(ctx, "token", r.f_decl.f_syn_name)
            )

            check_source_language(
                token_camel_name not in ('Termination', 'LexingFailure'),
                '{} is a reserved token name'.format(token_camel_name)
            )
            check_source_language(token_name not in tokens,
                                  'Duplicate token name')

            # Create the token action
            token: Action
            if token_kind in "text":
                token = WithText(
                    start_ignore_layout, end_ignore_layout, location
                )
            elif token_kind == "trivia":
                token = WithTrivia(
                    start_ignore_layout, end_ignore_layout, comment, location
                )
            elif token_kind == "symbol":
                token = WithSymbol(
                    start_ignore_layout, end_ignore_layout, location
                )
            else:
                assert token_kind == "ignored"
                token = Ignore(location)

            # Register it
            if token_name is not None:
                tokens[token_name] = token
            if isinstance(token, TokenAction):
                if token_set is not None:
                    token_set.add(token)
                if rule_annot.with_unparsing_newline:
                    newline_after.append(token)

            # If there is a matcher, register this rule to be processed later
            assert isinstance(r.f_decl, L.GrammarRuleDecl)
            matcher_expr = r.f_decl.f_expr
            if matcher_expr is not None:
                rules.append(RegularRule(token, is_pre, matcher_expr))

    def process_pattern(full_decl: L.FullDecl) -> None:
        """
        Process a pattern declaration.

        :param full_decl: Full declaration for the ValDecl to process.
        """
        check_no_annotations(full_decl)
        decl = full_decl.f_decl
        assert isinstance(decl, L.ValDecl)
        name = name_from_lower(ctx, "pattern", decl.f_syn_name)

        with lkt_context(decl):
            check_source_language(name not in patterns,
                                  'Duplicate pattern name')
            with lkt_context(decl.f_decl_type):
                check_source_language(
                    decl.f_decl_type is None,
                    "Types are not allowed in lexer declarations"
                )
            patterns[name] = (
                parse_static_pattern(ctx, decl.f_expr),
                Location.from_lkt_node(decl)
            )

    def lower_matcher_list(expr: L.GrammarExpr) -> list[Matcher]:
        """
        Lower a list of token matchers to our internals.

        Lists of token matchers are made up of "atomic" matchers nested in "or"
        grammar expressions.
        """
        if isinstance(expr, L.GrammarOrExpr):
            result = []
            for child_list in expr.f_sub_exprs:
                with lkt_context(child_list):
                    check_source_language(
                        len(child_list) == 1, "exactly one matcher expected"
                    )
                result += lower_matcher_list(child_list[0])
            return result
        else:
            return [lower_matcher(expr)]

    def lower_matcher(expr: L.GrammarExpr) -> Matcher:
        """
        Lower a token matcher to our internals.
        """
        loc = Location.from_lkt_node(expr)
        with lkt_context(expr):
            if isinstance(expr, L.TokenLit):
                return Literal(denoted_str(expr), location=loc)
            elif isinstance(expr, L.TokenNoCaseLit):
                return NoCaseLit(denoted_str(expr.f_lit), location=loc)
            elif isinstance(expr, (L.TokenPatternLit, L.TokenPatternConcat)):
                return Pattern(parse_static_pattern(ctx, expr), location=loc)
            else:
                error('Invalid lexing expression')

    def lower_token_ref(ref: L.RefId) -> Action:
        """
        Return the Token that `ref` refers to.
        """
        with lkt_context(ref):
            token_name = names.Name.check_from_camel(ref.text)
            check_source_language(token_name in tokens,
                                  'Unknown token: {}'.format(token_name.camel))
            return tokens[token_name]

    def lower_case_alt(alt: L.BaseLexerCaseRuleAlt) -> Alt:
        """
        Lower the alternative of a case lexing rule.
        """
        prev_token_cond = None
        if isinstance(alt, L.LexerCaseRuleCondAlt):
            prev_token_cond = [lower_token_ref(ref)
                               for ref in alt.f_cond_exprs]
        return Alt(prev_token_cond=prev_token_cond,
                   send=lower_token_ref(alt.f_send.f_sent),
                   match_size=int(alt.f_send.f_match_size.text))

    # First, go through all rules to register tokens and their token families.
    # Process lexing rules only after that (in order, to preserve precedence
    # information). Doing two passes is necessary to properly handle "forward
    # references".
    src_rules: list[SrcRule] = []
    for full_decl in full_lexer.f_decl.f_rules:
        with lkt_context(full_decl):
            if isinstance(full_decl, L.FullDecl):
                # There can be various types of declarations in lexers...
                decl = full_decl.f_decl

                if isinstance(decl, L.GrammarRuleDecl):
                    # Here, we have a token declaration, potentially associated
                    # with a lexing rule.
                    process_token_rule(full_decl, src_rules)

                elif isinstance(decl, L.ValDecl):
                    # This is the declaration of a pattern
                    process_pattern(full_decl)

                elif isinstance(decl, L.LexerFamilyDecl):
                    # This is a family block: go through all declarations
                    # inside it.
                    process_family(decl, src_rules)

                else:
                    check_source_language(False,
                                          'Unexpected declaration in lexer')

            elif isinstance(full_decl, L.LexerCaseRule):
                src_rules.append(full_decl)

            else:
                # The grammar should make the following dead code
                assert False, 'Invalid lexer rule: {}'.format(full_decl)

    # Lower all lexing rules in source order
    for r in src_rules:
        if isinstance(r, RegularRule):
            for matcher in lower_matcher_list(r.matcher_expr):
                rule = (matcher, r.token)
                if r.is_pre:
                    pre_rules.append(rule)
                else:
                    rules.append(rule)

        elif isinstance(r, L.LexerCaseRule):
            syn_alts = list(r.f_alts)
            with lkt_context(r):
                check_source_language(
                    len(syn_alts) == 2 and
                    isinstance(syn_alts[0], L.LexerCaseRuleCondAlt) and
                    isinstance(syn_alts[1], L.LexerCaseRuleDefaultAlt),
                    'Invalid case rule topology'
                )
            matcher_expr = r.f_expr
            matcher = lower_matcher(matcher_expr)
            rules.append(
                Case(
                    matcher,
                    lower_case_alt(syn_alts[0]),
                    lower_case_alt(syn_alts[1]),
                    location=Location.from_lkt_node(matcher_expr),
                )
            )

        else:
            assert False, f"Unexpected lexer rule: {r}"

    # Create the LexerToken subclass to define all tokens and token families
    items: dict[str, Action | TokenFamily] = {}
    for name, token in tokens.items():
        items[name.camel] = token
    for name, (token_set, loc) in token_family_sets.items():
        tf = TokenFamily(*list(token_set), location=loc)
        token_families[name] = tf
        items[name.camel] = tf
    token_class = type('Token', (LexerToken, ), items)

    # Create the Lexer instance and register all patterns and lexing rules
    result = Lexer(
        token_class,
        lexer_annot.indentation_tracking,
        pre_rules,
        lexer_annot.case_insensitive,
    )
    for name, (regexp, loc) in patterns.items():
        result._add_pattern(name.lower, regexp, location=loc)
    result.add_rules(*rules)

    # Register spacing/newline rules
    for f1_name, f2_ref in spacings:
        f2_name = names.Name.check_from_lower(f2_ref.text)
        with lkt_context(f2_ref):
            check_source_language(
                f2_name in token_families,
                'Unknown token family: {}'.format(f2_name.lower)
            )
        result.add_spacing((token_families[f1_name],
                            token_families[f2_name]))
    result.add_newline_after(*newline_after)

    return result
