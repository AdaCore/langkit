"""
Module to gather the logic to lower Lkt syntax trees to Langkit internal data
structures.
"""

from __future__ import annotations

from collections import OrderedDict
from dataclasses import dataclass
import itertools
import json
import os.path
from typing import (Any, ClassVar, Dict, List, Optional, Set, Tuple, Type,
                    TypeVar, Union, cast)

import liblktlang as L

from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, Argument, BaseField, CompiledType,
    CompiledTypeRepo, EnumNodeAlternative, EnumType, Field, StructType, T,
    TypeRepo, UserField, resolve_type
)
from langkit.diagnostics import (
    DiagnosticError, Location, check_source_language, error
)
import langkit.expressions as E
from langkit.expressions import (
    AbstractExpression, AbstractProperty, AbstractVariable, Property,
    PropertyDef
)
from langkit.lexer import (
    Action, Alt, Case, Ignore, Lexer, LexerToken, Literal, Matcher, NoCaseLit,
    Pattern, RuleAssoc, TokenAction, TokenFamily, WithSymbol, WithText,
    WithTrivia
)
import langkit.names as names
from langkit.parsers import (Discard, DontSkip, Grammar, List as PList, Null,
                             Opt, Or, Parser, Pick, Predicate, Skip, _Row,
                             _Token, _Transform)


CompiledTypeOrDefer = Union[CompiledType, TypeRepo.Defer]


# List of annotations that we don't compute here but that we can safely ignore
ANNOTATIONS_WHITELIST = ['builtin']


def get_trait(decl: L.TypeDecl, trait_name: str) -> Optional[L.TypeDecl]:
    """
    Return the trait named ``trait_name`` on declaration ``decl``.
    """
    for trait in decl.f_traits:
        trait_decl: L.TypeDecl = trait.p_designated_type
        if trait_decl.p_name == trait_name:
            return trait_decl
    return None


def check_referenced_decl(expr: L.Expr) -> L.Decl:
    """
    Wrapper around ``Expr.p_check_referenced_decl``.

    Since we are supposed to lower Lkt code only when it has no semantic error,
    this property should never fail. If it does, there is a bug somewhere in
    Langkit: raise an assertion error that points to the relevant Lkt node.
    """
    try:
        return expr.p_check_referenced_decl
    except L.PropertyError as exc:
        assert False, f"Cannot get referenced decl for {expr}: {exc}"


def pattern_as_str(str_lit: Union[L.StringLit, L.TokenPatternLit]) -> str:
    """
    Return the regexp string associated to this string literal node.
    """
    return json.loads(str_lit.text[1:])


def parse_static_bool(ctx: CompileCtx, expr: L.Expr) -> bool:
    """
    Return the bool value that this expression denotes.
    """
    with ctx.lkt_context(expr):
        check_source_language(isinstance(expr, L.RefId)
                              and expr.text in ('false', 'true'),
                              'Boolean literal expected')

    return expr.text == 'true'


def denoted_char_lit(char_lit: L.CharLit) -> str:
    """
    Return the character that ``char_lit`` denotes.
    """
    text = char_lit.text
    assert text[0] == "'" and text[-1] == "'"
    result = json.loads('"' + text[1:-1] + '"')
    assert len(result) == 1
    return result


def denoted_string_lit(string_lit: Union[L.StringLit, L.TokenLit]) -> str:
    """
    Return the string that ``string_lit`` denotes.
    """
    result = json.loads(string_lit.text)
    assert isinstance(result, str)
    return result


def load_lkt(lkt_file: str) -> List[L.AnalysisUnit]:
    """
    Load a Lktlang source file and return the closure of Lkt units referenced.
    Raise a DiagnosticError if there are parsing errors.

    :param lkt_file: Name of the file to parse.
    """
    units_map = OrderedDict()
    diagnostics = []

    def process_unit(unit: L.AnalysisUnit) -> None:
        if unit.filename in units_map:
            return

        # Register this unit and its diagnostics
        units_map[unit.filename] = unit
        for d in unit.diagnostics:
            diagnostics.append((unit, d))

        # Recursively process the units it imports. In case of parsing error,
        # just stop the recursion: the collection of diagnostics is enough.
        if not unit.diagnostics:
            assert isinstance(unit.root, L.LangkitRoot)
            import_stmts = list(unit.root.f_imports)
            for imp in import_stmts:
                process_unit(imp.p_referenced_unit)

    # Load ``lkt_file`` and all the units it references, transitively
    process_unit(L.AnalysisContext().get_from_file(lkt_file))

    # If there are diagnostics, forward them to the user. TODO: hand them to
    # langkit.diagnostic.
    if diagnostics:
        for u, d in diagnostics:
            print('{}:{}'.format(os.path.basename(u.filename), d))
        raise DiagnosticError()
    return list(units_map.values())


def find_toplevel_decl(ctx: CompileCtx,
                       lkt_units: List[L.AnalysisUnit],
                       node_type: type,
                       label: str) -> L.FullDecl:
    """
    Look for a top-level declaration of type ``node_type`` in the given units.

    If none or several are found, emit error diagnostics. Return the associated
    full declaration.

    :param lkt_units: List of units where to look.
    :param node_type: Node type to look for.
    :param label: Human readable string for what to look for. Used to create
        diagnostic mesages.
    """
    result = None
    for unit in lkt_units:
        assert isinstance(unit.root, L.LangkitRoot)
        for decl in unit.root.f_decls:
            if not isinstance(decl.f_decl, node_type):
                continue

            with ctx.lkt_context(decl):
                if result is not None:
                    check_source_language(
                        False,
                        'only one {} allowed (previous found at {}:{})'.format(
                            label,
                            os.path.basename(result.unit.filename),
                            result.sloc_range.start
                        )
                    )
                result = decl

    with ctx.lkt_context(lkt_units[0].root):
        if result is None:
            error('missing {}'.format(label))

    return result


class AnnotationSpec:
    """
    Synthetic description of how a declaration annotation works.
    """

    def __init__(self, name: str, unique: bool, require_args: bool,
                 default_value: Any = None):
        """
        :param name: Name of the annotation (``foo`` for the ``@foo``
            annotation).
        :param unique: Whether this annotation can appear at most once for a
            given declaration.
        :param require_args: Whether this annotation requires arguments.
        :param default_value: For unique annotations, value to use in case the
            annotation is absent.
        """
        self.name = name
        self.unique = unique
        self.require_args = require_args
        self.default_value = default_value if unique else []

    def interpret(self, ctx: CompileCtx,
                  args: List[L.Expr],
                  kwargs: Dict[str, L.Expr]) -> Any:
        """
        Subclasses must override this in order to interpret an annotation.

        This method must validate and interpret ``args`` and ``kwargs``, and
        return a value suitable for annotations processing.

        :param args: Positional arguments for the annotation.
        :param kwargs: Keyword arguments for the annotation.
        """
        raise NotImplementedError

    def parse_single_annotation(self,
                                ctx: CompileCtx,
                                result: Dict[str, Any],
                                annotation: L.DeclAnnotation) -> None:
        """
        Parse an annotation node according to this spec. Add the result to
        ``result``.
        """
        check_source_language(
            self.name not in result or not self.unique,
            'This annotation cannot appear multiple times'
        )

        # Check that parameters presence comply to the spec
        if not annotation.f_params:
            check_source_language(not self.require_args,
                                  'Arguments required for this annotation')
            value = self.interpret(ctx, [], {})
        else:
            check_source_language(self.require_args,
                                  'This annotation accepts no argument')

            # Collect positional and named arguments
            args = []
            kwargs = {}
            for param in annotation.f_params.f_params:
                with ctx.lkt_context(param):
                    if param.f_name:
                        name = param.f_name.text
                        check_source_language(name not in kwargs,
                                              'Named argument repeated')
                        kwargs[name] = param.f_value

                    else:
                        check_source_language(not kwargs,
                                              'Positional arguments must'
                                              ' appear before named ones')
                        args.append(param.f_value)

            # Evaluate this annotation
            value = self.interpret(ctx, args, kwargs)

        # Store annotation evaluation into the result
        if self.unique:
            result[self.name] = value
        else:
            result.setdefault(self.name, [])
            result[self.name].append(value)


class FlagAnnotationSpec(AnnotationSpec):
    """
    Convenience subclass for flags.
    """
    def __init__(self, name: str):
        super().__init__(name, unique=True, require_args=False,
                         default_value=False)

    def interpret(self,
                  ctx: CompileCtx,
                  args: List[L.Expr],
                  kwargs: Dict[str, L.Expr]) -> Any:
        return True


class SpacingAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @spacing annotations for token families.
    """
    def __init__(self) -> None:
        super().__init__('unparse_spacing', unique=False, require_args=True)

    def interpret(self,
                  ctx: CompileCtx,
                  args: List[L.Expr],
                  kwargs: Dict[str, L.Expr]) -> L.RefId:
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


class TokenAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @text/symbol/trivia annotations for tokens.
    """
    def __init__(self, name: str):
        super().__init__(name, unique=True, require_args=True)

    def interpret(self,
                  ctx: CompileCtx,
                  args: List[L.Expr],
                  kwargs: Dict[str, L.Expr]) -> Tuple[bool, bool]:
        check_source_language(not args, 'No positional argument allowed')

        try:
            expr = kwargs.pop('start_ignore_layout')
        except KeyError:
            start_ignore_layout = False
        else:
            start_ignore_layout = parse_static_bool(ctx, expr)

        try:
            expr = kwargs.pop('end_ignore_layout')
        except KeyError:
            end_ignore_layout = False
        else:
            end_ignore_layout = parse_static_bool(ctx, expr)

        check_source_language(
            not kwargs,
            'Invalid arguments: {}'.format(', '.join(sorted(kwargs)))
        )

        return (start_ignore_layout, end_ignore_layout)


class WithLexerAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_lexer annotations for grammar declarations.
    """
    def __init__(self) -> None:
        super().__init__('with_lexer', unique=True, require_args=True)

    def interpret(self,
                  ctx: CompileCtx,
                  args: List[L.Expr],
                  kwargs: Dict[str, L.Expr]) -> L.LexerDecl:
        check_source_language(not kwargs, 'No keyword argument allowed')
        check_source_language(len(args) == 1, 'Exactly one argument expected')

        lexer_ref = args[0]
        if not isinstance(lexer_ref, L.RefId):
            error('Invalid lexer reference')

        # TODO: this will raise a PropertyError if semantic resolution fails.
        # Resolving this lexer reference should be done in the check pass so
        # that we never land here in that case.
        lexer_decl = check_referenced_decl(lexer_ref)
        if not isinstance(lexer_decl, L.LexerDecl):
            error('Lexer expected, got {}'.format(lexer_decl))

        return lexer_decl


token_cls_map = {'text': WithText,
                 'trivia': WithTrivia,
                 'symbol': WithSymbol}


@dataclass
class ParsedAnnotations:
    """
    Namespace object to hold annotation parsed values.
    """

    annotations: ClassVar[List[AnnotationSpec]]


@dataclass
class GrammarAnnotations(ParsedAnnotations):
    with_lexer: L.LexerDecl
    annotations = [WithLexerAnnotationSpec()]


@dataclass
class GrammarRuleAnnotations(ParsedAnnotations):
    main_rule: bool
    annotations = [FlagAnnotationSpec('main_rule')]


@dataclass
class TokenAnnotations(ParsedAnnotations):
    text: Tuple[bool, bool]
    trivia: Tuple[bool, bool]
    symbol: Tuple[bool, bool]
    unparse_newline_after: bool
    pre_rule: bool
    ignore: bool
    annotations = [TokenAnnotationSpec('text'),
                   TokenAnnotationSpec('trivia'),
                   TokenAnnotationSpec('symbol'),
                   FlagAnnotationSpec('unparse_newline_after'),
                   FlagAnnotationSpec('pre_rule'),
                   FlagAnnotationSpec('ignore')]


@dataclass
class LexerAnnotations(ParsedAnnotations):
    track_indent: bool
    annotations = [FlagAnnotationSpec('track_indent')]


@dataclass
class TokenFamilyAnnotations(ParsedAnnotations):
    unparse_spacing: List[L.RefId]
    annotations = [SpacingAnnotationSpec()]


@dataclass
class BaseNodeAnnotations(ParsedAnnotations):
    has_abstract_list: bool
    annotations = [FlagAnnotationSpec('has_abstract_list')]


@dataclass
class NodeAnnotations(BaseNodeAnnotations):
    abstract: bool
    annotations = BaseNodeAnnotations.annotations + [
        FlagAnnotationSpec('abstract')
    ]


@dataclass
class EnumNodeAnnotations(BaseNodeAnnotations):
    qualifier: bool
    annotations = BaseNodeAnnotations.annotations + [
        FlagAnnotationSpec('qualifier')
    ]


@dataclass
class FieldAnnotations(ParsedAnnotations):
    abstract: bool
    null_field: bool
    parse_field: bool
    annotations = [FlagAnnotationSpec('abstract'),
                   FlagAnnotationSpec('null_field'),
                   FlagAnnotationSpec('parse_field')]


@dataclass
class EnumAnnotations(ParsedAnnotations):
    annotations: ClassVar[List[AnnotationSpec]] = []


@dataclass
class StructAnnotations(ParsedAnnotations):
    annotations: ClassVar[List[AnnotationSpec]] = []


@dataclass
class FunAnnotations(ParsedAnnotations):
    abstract: bool
    export: bool
    annotations = [
        FlagAnnotationSpec('abstract'),

        FlagAnnotationSpec('export'),
    ]


def check_no_annotations(full_decl: L.FullDecl) -> None:
    """
    Check that the declaration has no annotation.
    """
    check_source_language(
        len(full_decl.f_decl_annotations) == 0, 'No annotation allowed'
    )


AnyPA = TypeVar('AnyPA', bound=ParsedAnnotations)


def parse_annotations(ctx: CompileCtx,
                      annotation_class: Type[AnyPA],
                      full_decl: L.FullDecl) -> AnyPA:
    """
    Parse annotations according to the specs in
    ``annotation_class.annotations``. Return a ParsedAnnotations that contains
    the interpreted annotation values for each present annotation.

    :param annotation_class: ParsedAnnotations subclass for the result, holding
        the annotation specs to guide parsing.
    :param full_decl: Declaration whose annotations are to be parsed.
    """
    # Build a mapping for all specs
    specs_map: Dict[str, AnnotationSpec] = {}
    for s in annotation_class.annotations:
        assert s.name not in specs_map
        specs_map[s.name] = s

    # Process annotations
    values: Dict[str, Any] = {}
    for a in full_decl.f_decl_annotations:
        name = a.f_name.text
        spec = specs_map.get(name)
        with ctx.lkt_context(a):
            if spec is None:
                if name not in ANNOTATIONS_WHITELIST:
                    check_source_language(
                        False, 'Invalid annotation: {}'.format(name)
                    )
            else:
                spec.parse_single_annotation(ctx, values, a)

    # Use the default value for absent annotations
    for s in annotation_class.annotations:
        values.setdefault(s.name, s.default_value)

    # Create the namespace object to hold results
    return annotation_class(**values)  # type: ignore


def create_lexer(ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]) -> Lexer:
    """
    Create and populate a lexer from a Lktlang unit.

    :param lkt_units: Non-empty list of analysis units where to look for the
        grammar.
    """
    # Look for the LexerDecl node in top-level lists
    full_lexer = find_toplevel_decl(ctx, lkt_units, L.LexerDecl, 'lexer')
    assert isinstance(full_lexer.f_decl, L.LexerDecl)

    with ctx.lkt_context(full_lexer):
        lexer_annot = parse_annotations(ctx, LexerAnnotations, full_lexer)

    patterns: Dict[names.Name, str] = {}
    """
    Mapping from pattern names to the corresponding regular expression.
    """

    token_family_sets: Dict[names.Name, Set[TokenAction]] = {}
    """
    Mapping from token family names to the corresponding sets of tokens that
    belong to this family.
    """

    token_families: Dict[names.Name, TokenFamily] = {}
    """
    Mapping from token family names to the corresponding token families.  We
    build this late, once we know all tokens and all families.
    """

    spacings: List[Tuple[names.Name, L.RefId]] = []
    """
    Couple of names for token family between which unparsing must insert
    spaces. The first name is known to be valid, but not the second one, so we
    keep it as a node to create a diagnostic context.
    """

    tokens: Dict[names.Name, Action] = {}
    """
    Mapping from token names to the corresponding tokens.
    """

    rules: List[Union[RuleAssoc, Tuple[Matcher, Action]]] = []
    pre_rules: List[Tuple[Matcher, Action]] = []
    """
    Lists of regular and pre lexing rules for this lexer.
    """

    newline_after: List[TokenAction] = []
    """
    List of tokens after which we must introduce a newline during unparsing.
    """

    def ignore_constructor(start_ignore_layout: bool,
                           end_ignore_layout: bool) -> Action:
        """
        Adapter to build a Ignore instance with the same API as WithText
        constructors.
        """
        del start_ignore_layout, end_ignore_layout
        return Ignore()

    def process_family(f: L.LexerFamilyDecl) -> None:
        """
        Process a LexerFamilyDecl node. Register the token family and process
        the rules it contains.
        """
        with ctx.lkt_context(f):
            # Create the token family, if needed
            name = names.Name.from_lower(f.f_syn_name.text)
            token_set = token_family_sets.setdefault(name, set())

            for r in f.f_rules:
                if not isinstance(r.f_decl, L.GrammarRuleDecl):
                    error('Only lexer rules allowed in family blocks')
                process_token_rule(r, token_set)

            family_annotations = parse_annotations(ctx, TokenFamilyAnnotations,
                                                   cast(L.FullDecl, f.parent))

            for spacing in family_annotations.unparse_spacing:
                spacings.append((name, spacing))

    def process_token_rule(
        r: L.FullDecl,
        token_set: Optional[Set[TokenAction]] = None
    ) -> None:
        """
        Process the full declaration of a GrammarRuleDecl node: create the
        token it declares and lower the optional associated lexing rule.

        :param r: Full declaration for the GrammarRuleDecl to process.
        :param token_set: If this declaration appears in the context of a token
            family, this adds the new token to this set.  Must be left to None
            otherwise.
        """
        with ctx.lkt_context(r):
            rule_annot: TokenAnnotations = parse_annotations(
                ctx, TokenAnnotations, r
            )

            # Gather token action info from the annotations. If absent,
            # fallback to WithText.
            token_cons = None
            start_ignore_layout = False
            end_ignore_layout = False
            if rule_annot.ignore:
                token_cons = ignore_constructor
            for name in ('text', 'trivia', 'symbol'):
                annot = getattr(rule_annot, name)
                if not annot:
                    continue
                start_ignore_layout, end_ignore_layout = annot

                check_source_language(token_cons is None,
                                      'At most one token action allowed')
                token_cons = token_cls_map[name]
            is_pre = rule_annot.pre_rule
            if token_cons is None:
                token_cons = WithText

            # Create the token and register it where needed: the global token
            # mapping, its token family (if any) and the "newline_after" group
            # if the corresponding annotation is present.
            token_lower_name = r.f_decl.f_syn_name.text
            token_name = names.Name.from_lower(token_lower_name)

            check_source_language(
                token_lower_name not in ('termination', 'lexing_failure'),
                '{} is a reserved token name'.format(token_lower_name)
            )
            check_source_language(token_name not in tokens,
                                  'Duplicate token name')

            token = token_cons(start_ignore_layout, end_ignore_layout)
            tokens[token_name] = token
            if isinstance(token, TokenAction):
                if token_set is not None:
                    token_set.add(token)
                if rule_annot.unparse_newline_after:
                    newline_after.append(token)

            # Lower the lexing rule, if present
            assert isinstance(r.f_decl, L.GrammarRuleDecl)
            matcher_expr = r.f_decl.f_expr
            if matcher_expr is not None:
                rule = (lower_matcher(matcher_expr), token)
                if is_pre:
                    pre_rules.append(rule)
                else:
                    rules.append(rule)

    def process_pattern(full_decl: L.FullDecl) -> None:
        """
        Process a pattern declaration.

        :param full_decl: Full declaration for the ValDecl to process.
        """
        check_no_annotations(full_decl)
        decl = full_decl.f_decl
        assert isinstance(decl, L.ValDecl)
        lower_name = decl.f_syn_name.text
        name = names.Name.from_lower(lower_name)

        with ctx.lkt_context(decl):
            check_source_language(name not in patterns,
                                  'Duplicate pattern name')
            check_source_language(decl.f_decl_type is None,
                                  'Patterns must have automatic types in'
                                  ' lexers')
            if (
                not isinstance(decl.f_val, L.StringLit)
                or not decl.f_val.p_is_regexp_literal
            ):
                error('Pattern string literal expected')
            # TODO: use StringLit.p_denoted_value when properly implemented
            patterns[name] = pattern_as_str(decl.f_val)

    def lower_matcher(expr: L.GrammarExpr) -> Matcher:
        """
        Lower a token matcher to our internals.
        """
        with ctx.lkt_context(expr):
            if isinstance(expr, L.TokenLit):
                return Literal(json.loads(expr.text))
            elif isinstance(expr, L.TokenNoCaseLit):
                return NoCaseLit(json.loads(expr.text))
            elif isinstance(expr, L.TokenPatternLit):
                return Pattern(pattern_as_str(expr))
            else:
                error('Invalid lexing expression')

    def lower_token_ref(ref: L.RefId) -> Action:
        """
        Return the Token that `ref` refers to.
        """
        with ctx.lkt_context(ref):
            token_name = names.Name.from_lower(ref.text)
            check_source_language(token_name in tokens,
                                  'Unknown token: {}'.format(token_name.lower))
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

    # Go through all rules to register tokens, their token families and lexing
    # rules.
    for full_decl in full_lexer.f_decl.f_rules:
        with ctx.lkt_context(full_decl):
            if isinstance(full_decl, L.FullDecl):
                # There can be various types of declarations in lexers...
                decl = full_decl.f_decl

                if isinstance(decl, L.GrammarRuleDecl):
                    # Here, we have a token declaration, potentially associated
                    # with a lexing rule.
                    process_token_rule(full_decl)

                elif isinstance(decl, L.ValDecl):
                    # This is the declaration of a pattern
                    process_pattern(full_decl)

                elif isinstance(decl, L.LexerFamilyDecl):
                    # This is a family block: go through all declarations
                    # inside it.
                    process_family(decl)

                else:
                    check_source_language(False,
                                          'Unexpected declaration in lexer')

            elif isinstance(full_decl, L.LexerCaseRule):
                syn_alts = list(full_decl.f_alts)

                # This is a rule for conditional lexing: lower its matcher and
                # its alternative rules.
                matcher = lower_matcher(full_decl.f_expr)
                check_source_language(
                    len(syn_alts) == 2 and
                    isinstance(syn_alts[0], L.LexerCaseRuleCondAlt) and
                    isinstance(syn_alts[1], L.LexerCaseRuleDefaultAlt),
                    'Invalid case rule topology'
                )
                rules.append(Case(matcher,
                                  lower_case_alt(syn_alts[0]),
                                  lower_case_alt(syn_alts[1])))

            else:
                # The grammar should make the following dead code
                assert False, 'Invalid lexer rule: {}'.format(full_decl)

    # Create the LexerToken subclass to define all tokens and token families
    items: Dict[str, Union[Action, TokenFamily]] = {}
    for name, token in tokens.items():
        items[name.camel] = token
    for name, token_set in token_family_sets.items():
        tf = TokenFamily(*list(token_set))
        token_families[name] = tf
        items[name.camel] = tf
    token_class = type('Token', (LexerToken, ), items)

    # Create the Lexer instance and register all patterns and lexing rules
    result = Lexer(token_class,
                   lexer_annot.track_indent,
                   pre_rules)
    for name, regexp in patterns.items():
        result.add_patterns((name.lower, regexp))
    result.add_rules(*rules)

    # Register spacing/newline rules
    for f1_name, f2_ref in spacings:
        f2_name = names.Name.from_lower(f2_ref.text)
        with ctx.lkt_context(f2_ref):
            check_source_language(
                f2_name in token_families,
                'Unknown token family: {}'.format(f2_name.lower)
            )
        result.add_spacing((token_families[f1_name],
                            token_families[f2_name]))
    result.add_newline_after(*newline_after)

    return result


def create_grammar(ctx: CompileCtx,
                   lkt_units: List[L.AnalysisUnit]) -> Grammar:
    """
    Create a grammar from a set of Lktlang units.

    Note that this only initializes a grammar and fetches relevant declarations
    in the Lktlang unit. The actual lowering on grammar rules happens in a
    separate pass: see lower_all_lkt_rules.

    :param lkt_units: Non-empty list of analysis units where to look for the
        grammar.
    """
    # Look for the GrammarDecl node in top-level lists
    full_grammar = find_toplevel_decl(ctx, lkt_units, L.GrammarDecl, 'grammar')
    assert isinstance(full_grammar.f_decl, L.GrammarDecl)

    with ctx.lkt_context(full_grammar):
        parse_annotations(ctx, GrammarAnnotations, full_grammar)

    # Get the list of grammar rules. This is where we check that we only have
    # grammar rules, that their names are unique, and that they have valid
    # annotations.
    all_rules = OrderedDict()
    main_rule_name = None
    for full_rule in full_grammar.f_decl.f_rules:
        with ctx.lkt_context(full_rule):
            r = full_rule.f_decl

            # Make sure we have a grammar rule
            if not isinstance(r, L.GrammarRuleDecl):
                error('grammar rule expected')
            rule_name = r.f_syn_name.text

            # Register the main rule if the appropriate annotation is present
            anns = parse_annotations(ctx, GrammarRuleAnnotations, full_rule)
            if anns.main_rule:
                check_source_language(main_rule_name is None,
                                      'only one main rule allowed')
                main_rule_name = rule_name

            all_rules[rule_name] = r.f_expr

    # Now create the result grammar. We need exactly one main rule for that.
    with ctx.lkt_context(full_grammar):
        check_source_language(main_rule_name is not None,
                              'Missing main rule (@main_rule annotation)')
    result = Grammar(main_rule_name, Location.from_lkt_node(full_grammar))

    # Translate rules (all_rules) later, as node types are not available yet
    result._all_lkt_rules.update(all_rules)
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

    # Build a mapping for all tokens registered in the lexer. Use lower case
    # names, as this is what the concrete syntax is supposed to use.
    tokens = {cast(names.Name, token.name).lower: token
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
        node_ref: Optional[NodeRefTypes]
    ) -> Optional[ASTNodeType]:
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
            with ctx.lkt_context(node_ref.f_prefix):
                try:
                    alt_map = enum_nodes[prefix_node]
                except KeyError:
                    error('Non-qualifier enum node expected (got {})'
                          .format(prefix_node.dsl_name))

            # Then resolve the alternative
            suffix = node_ref.f_suffix.text
            with ctx.lkt_context(node_ref.f_suffix):
                try:
                    return alt_map[suffix]
                except KeyError:
                    error('Unknown enum node alternative: {}'.format(suffix))

        elif isinstance(node_ref, L.GenericTypeRef):
            check_source_language(
                node_ref.f_type_name.text == u'ASTList',
                'Bad generic type name: only ASTList is valid in this context'
            )

            params = node_ref.f_params
            check_source_language(
                len(params) == 1,
                '1 type argument expected, got {}'.format(len(params))
            )
            return resolve_node_ref(cast(NodeRefTypes, params[0])).list

        elif isinstance(node_ref, L.SimpleTypeRef):
            return resolve_node_ref(cast(NodeRefTypes, node_ref.f_type_name))

        else:
            assert isinstance(node_ref, L.RefId)
            with ctx.lkt_context(node_ref):
                node_name = node_ref.text
                try:
                    return nodes[node_name]
                except KeyError:
                    error('Unknown node: {}'.format(node_name))

        raise RuntimeError("unreachable code")

    def lower(
        rule: Union[L.GrammarExpr, L.GrammarExprList]
    ) -> Optional[Parser]:
        """
        Helper to lower one parser.

        :param rule: Grammar rule to lower.
        """
        # For convenience, accept null input rules, as we generally want to
        # forward them as-is to the lower level parsing machinery.
        if rule is None:
            return None

        loc = Location.from_lkt_node(rule)
        with ctx.lkt_context(rule):
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
                    check_source_language(
                        False, 'Unknown token: {}'.format(token_name)
                    )

                match_text = ''
                if rule.f_expr:
                    # The grammar is supposed to mainain this invariant
                    assert isinstance(rule.f_expr, L.TokenLit)
                    match_text = denoted_string_lit(rule.f_expr)

                return _Token(val=val, match_text=match_text, location=loc)

            elif isinstance(rule, L.TokenLit):
                return _Token(denoted_string_lit(rule), location=loc)

            elif isinstance(rule, L.GrammarList):
                return PList(
                    lower(rule.f_expr),
                    empty_valid=rule.f_kind.text == '*',
                    list_cls=resolve_node_ref_or_none(rule.f_list_type),
                    sep=lower(rule.f_sep),
                    location=loc
                )

            elif isinstance(rule, (L.GrammarImplicitPick,
                                   L.GrammarPick)):
                return Pick(*[lower(subparser) for subparser in rule.f_exprs],
                            location=loc)

            elif isinstance(rule, L.GrammarRuleRef):
                return getattr(grammar, rule.f_node_name.text)

            elif isinstance(rule, L.GrammarOrExpr):
                return Or(*[lower(subparser)
                            for subparser in rule.f_sub_exprs],
                          location=loc)

            elif isinstance(rule, L.GrammarOpt):
                return Opt(lower(rule.f_expr), location=loc)

            elif isinstance(rule, L.GrammarOptGroup):
                return Opt(*[lower(subparser) for subparser in rule.f_expr],
                           location=loc)

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

            elif isinstance(rule, L.GrammarPredicate):
                if not isinstance(rule.f_prop_ref, L.DotExpr):
                    error('Invalid property reference')
                node = resolve_node_ref(cast(NodeRefTypes,
                                             rule.f_prop_ref.f_prefix))
                prop_name = rule.f_prop_ref.f_suffix.text
                try:
                    prop = node.get_abstract_node_data_dict()[prop_name]
                except KeyError:
                    check_source_language(
                        False,
                        '{} has no {} property'
                        .format(node.dsl_name, prop_name)
                    )
                return Predicate(lower(rule.f_expr), prop, location=loc)

            else:
                raise NotImplementedError('unhandled parser: {}'.format(rule))

    for name, rule in grammar._all_lkt_rules.items():
        grammar._add_rule(name, lower(rule))


# Mapping to associate declarations to the corresponding AbstractVariable
# instances. This is useful when lowering expressions.
LocalsEnv = Dict[L.BaseValDecl, AbstractVariable]


class LktTypesLoader:
    """
    Helper class to instanciate ``CompiledType`` for all types described in
    Lkt.
    """

    # Map Lkt type declarations to the corresponding CompiledType instances, or
    # to None when the type declaration is currently being lowered. Keeping a
    # None entry in this case helps detecting illegal circular type
    # dependencies.
    compiled_types: Dict[L.TypeDecl, Optional[CompiledType]]

    # Map Lkt type declarations to TypeRepo.Defer instances that resolve to the
    # corresponding CompiledType instances.
    type_refs: Dict[L.TypeDecl, TypeRepo.Defer]

    def __init__(self, ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]):
        """
        :param ctx: Context in which to create these types.
        :param lkt_units: Non-empty list of analysis units where to look for
            type declarations.
        """
        self.ctx = ctx
        self.type_refs = {}

        root = lkt_units[0].root

        def get_field(decl: L.TraitDecl, name: str) -> L.Decl:
            """
            Return the (assumed existing and unique) declaration called
            ``name`` nested in ``decl``.
            """
            decls = [fd.f_decl
                     for fd in decl.f_decls
                     if fd.f_decl.p_name == name]
            assert len(decls) == 1, str(decls)
            return decls[0]

        # Pre-fetch the declaration of generic types so that resolve_type_decl
        # has an efficient access to them.
        self.array_type = root.p_array_type
        self.astlist_type = root.p_astlist_type
        self.iterator_trait = root.p_iterator_trait
        self.map_method = get_field(self.iterator_trait, 'map')
        self.analysis_unit_trait = root.p_analysis_unit_trait

        # Map Lkt nodes for the declarations of builtin types to the
        # corresponding CompiledType instances.
        self.compiled_types = {
            root.p_char_type: T.Character,
            root.p_int_type: T.Int,
            root.p_bool_type: T.Bool,
            root.p_bigint_type: T.BigInt,
            root.p_string_type: T.String,
            root.p_symbol_type: T.Symbol,
        }

        # Go through all units, build a map for all type definitions, indexed
        # by name. This first pass allows to check for type name unicity.
        named_type_decls: Dict[str, L.FullDecl] = {}
        for unit in lkt_units:
            assert isinstance(unit.root, L.LangkitRoot)
            for full_decl in unit.root.f_decls:
                if not isinstance(full_decl.f_decl, L.TypeDecl):
                    continue
                name = full_decl.f_decl.f_syn_name.text
                check_source_language(
                    name not in named_type_decls,
                    'Duplicate type name: {}'.format(name)
                )
                named_type_decls[name] = full_decl

        # Now create CompiledType instances for each user type. To properly
        # handle node derivation, this recurses on bases first and reject
        # inheritance loops.
        for _, decl in sorted(named_type_decls.items()):
            assert isinstance(decl.f_decl, L.TypeDecl)
            self.lower_type_decl(decl.f_decl)

    def resolve_type_decl(self, decl: L.TypeDecl) -> CompiledTypeOrDefer:
        """
        Fetch the CompiledType instance corresponding to the given type
        declaration. If it's not lowered yet, return an appropriate
        TypeRepo.Defer instance instead.

        :param decl: Lkt type declaration to resolve.
        """
        result: Optional[CompiledTypeOrDefer]

        # First, look for an actual CompiledType instance
        result = self.compiled_types.get(decl)
        if result is not None:
            return result

        # Not found: look now for an existing TypeRepo.Defer instance
        result = self.type_refs.get(decl)
        if result is not None:
            return result

        # Not found neither: create the TypeRepo.Defer instance
        if isinstance(decl, L.InstantiatedGenericType):
            inner_type = decl.p_get_inner_type
            actuals = decl.p_get_actuals

            if inner_type == self.array_type:
                assert len(actuals) == 1
                result = self.resolve_type_decl(actuals[0]).array

            elif inner_type == self.iterator_trait:
                assert len(actuals) == 1
                result = self.resolve_type_decl(actuals[0]).iterator

            elif inner_type == self.astlist_type:
                assert len(actuals) == 1
                node = self.resolve_type_decl(actuals[0])
                assert isinstance(node, (ASTNodeType, TypeRepo.Defer))
                result = node.list

            elif inner_type == self.analysis_unit_trait:
                result = T.AnalysisUnit

            else:
                assert False, (
                    'Unknown generic type: {} (from {})'
                    .format(inner_type, decl)
                )

        else:
            assert isinstance(decl, L.NamedTypeDecl)
            result = getattr(T, decl.f_syn_name.text)

        if isinstance(result, TypeRepo.Defer):
            self.type_refs[decl] = result
        else:
            assert isinstance(result, CompiledType)
            self.compiled_types[decl] = result
        return result

    def lower_type_decl(self, decl: L.TypeDecl) -> CompiledType:
        """
        Create the CompiledType instance corresponding to the given Lkt type
        declaration. Do nothing if it has been already lowered, and stop with
        an error if the lowering for this type is already running (case of
        invalid circular type dependency).
        """
        with self.ctx.lkt_context(decl):
            # Sentinel for the dict lookup below, as compiled_type can contain
            # None entries.
            try:
                t = self.compiled_types[decl]
            except KeyError:
                # The type is not lowered yet: let's do it
                pass
            else:
                if t is None:
                    error('Type inheritance loop detected')
                else:
                    # The type is already lowered: there is nothing to do
                    return t

            check_source_language(
                isinstance(decl, L.BasicClassDecl)
                or decl.f_traits is None
                or len(decl.f_traits) == 0,
                'No traits allowed except on nodes'
            )

            # Dispatch now to the appropriate lowering helper
            result: CompiledType
            if isinstance(decl, L.InstantiatedGenericType):
                # At this stage, the only generic types should come from the
                # prelude (Array, ASTList), so there is no need to do anything
                # special for them: just let the resolution mechanism build the
                # CompiledType through CompiledType attribute access magic.
                result = resolve_type(self.resolve_type_decl(decl))
            else:
                full_decl = decl.parent
                assert isinstance(full_decl, L.FullDecl)
                if isinstance(decl, L.BasicClassDecl):

                    specs = (EnumNodeAnnotations
                             if isinstance(decl, L.EnumClassDecl)
                             else NodeAnnotations)
                    result = self.create_node(
                        decl, parse_annotations(self.ctx, specs, full_decl)
                    )

                elif isinstance(decl, L.EnumTypeDecl):
                    result = self.create_enum(
                        decl,
                        parse_annotations(self.ctx, EnumAnnotations, full_decl)
                    )

                elif isinstance(decl, L.StructDecl):
                    result = self.create_struct(
                        decl,
                        parse_annotations(
                            self.ctx, StructAnnotations, full_decl
                        )
                    )

                else:
                    raise NotImplementedError(
                        'Unhandled type declaration: {}'.format(decl)
                    )

            self.compiled_types[decl] = result
            return result

    def lower_base_field(
        self,
        full_decl: L.FullDecl,
        allowed_field_types: Tuple[Type[AbstractNodeData], ...]
    ) -> BaseField:
        """
        Lower the BaseField described in ``decl``.

        :param allowed_field_types: Set of types allowed for the fields to
            load.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FieldDecl)
        annotations = parse_annotations(self.ctx, FieldAnnotations, full_decl)
        field_type = self.resolve_type_decl(decl.f_decl_type.p_designated_type)

        cls: Type[BaseField]
        kwargs: Dict[str, Any]
        default_value = (self.lower_expr(decl.f_default_val, {})
                         if decl.f_default_val else None)

        if annotations.parse_field:
            cls = Field
            kwargs = {'abstract': annotations.abstract,
                      'null': annotations.null_field}
            assert default_value is None
        else:
            check_source_language(
                not annotations.abstract,
                'Regular fields cannot be abstract'
            )
            check_source_language(
                not annotations.null_field,
                'Regular fields cannot be null'
            )
            cls = UserField
            kwargs = {'default_value': default_value}

        check_source_language(
            issubclass(cls, allowed_field_types),
            'Invalid field type in this context'
        )

        return cls(type=field_type, doc=self.ctx.lkt_doc(full_decl), **kwargs)

    def lower_expr(self, expr: L.Expr, env: LocalsEnv) -> AbstractExpression:
        """
        Lower the given expression.

        :param expr: Expression to lower.
        :param env: Variable to use when resolving references.
        """
        # Counter to generate unique names
        counter = itertools.count(0)

        def var_for_lambda_arg(
            arg: L.LambdaArgDecl,
            prefix: str,
            type: Optional[CompiledType] = None
        ) -> AbstractVariable:
            """
            Create an AbstractVariable to translate a lambda argument.

            This also registers this decl/variable association in ``env``.

            :param prefix: Lower-case prefix for the name of the variable in
                the generated code.
            """
            assert arg not in env
            source_name = names.Name.from_lower(arg.f_syn_name.text)
            result = AbstractVariable(
                names.Name.from_lower('{}_{}'.format(prefix, next(counter))),
                source_name=source_name,
                type=type,
            )
            env[arg] = result
            return result

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Do the actual expression lowering. Since all recursive calls use
            the same environment, this helper allows to skip passing it.
            """
            result: AbstractExpression

            if isinstance(expr, L.ArrayLiteral):
                elts = [lower(e) for e in expr.f_exprs]
                array_type = self.resolve_type_decl(expr.p_check_expr_type)
                return E.ArrayLiteral(elts,
                                      element_type=array_type.element_type)

            elif isinstance(expr, L.BinOp):
                # Lower both operands
                left = lower(expr.f_left)
                right = lower(expr.f_right)

                # Dispatch to the appropriate abstract expression constructor
                if isinstance(expr.f_op, L.OpEq):
                    return E.Eq(left, right)

                elif isinstance(expr.f_op, (L.OpLt, L.OpGt, L.OpLte, L.OpGte)):
                    operator = {
                        L.OpLt: E.OrderingTest.LT,
                        L.OpLte: E.OrderingTest.LE,
                        L.OpGt: E.OrderingTest.GT,
                        L.OpGte: E.OrderingTest.GE,
                    }[type(expr.f_op)]
                    return E.OrderingTest(operator, left, right)

                else:
                    operator = {
                        L.OpAmp: '&',
                        L.OpAnd: '&',
                        L.OpOr: '|',
                        L.OpPlus: '+',
                        L.OpMinus: '-',
                        L.OpMult: '*',
                        L.OpDiv: '/',
                    }[type(expr.f_op)]
                    return E.Arithmetic(left, right, operator)

            elif isinstance(expr, L.CallExpr):
                # Depending on its name, a call can have different meanings
                name_decl = check_referenced_decl(expr.f_name)
                call_expr = expr

                def lower_args() -> Tuple[List[AbstractExpression],
                                          Dict[str, AbstractExpression]]:
                    """
                    Collect call positional and keyword arguments.
                    """
                    args = []
                    kwargs = {}
                    for arg in call_expr.f_args:
                        value = lower(arg.f_value)
                        if arg.f_name:
                            kwargs[arg.f_name.text] = value
                        else:
                            args.append(value)
                    return args, kwargs

                if isinstance(name_decl, L.StructDecl):
                    # If the name refers to a struct type, this expression
                    # create a struct value.
                    struct_type = self.resolve_type_decl(name_decl)
                    args, kwargs = lower_args()
                    assert not args
                    return E.New(struct_type, **kwargs)

                # TODO: change liblktlang so that we get an object similar to
                # InstantiatedGenericType, and have an easy way to check it's
                # the builtin map function.
                elif (name_decl.unit, name_decl.sloc_range) == (
                    self.map_method.unit, self.map_method.sloc_range
                ):
                    # Build variable for iteration variables from the lambda
                    # expression arguments.
                    assert len(call_expr.f_args) == 1
                    lambda_expr = call_expr.f_args[0].f_value
                    assert isinstance(lambda_expr, L.LambdaExpr)
                    lambda_args = lambda_expr.f_params

                    # We expect either one argument (for the collection
                    # element) or two arguments (the collection element and the
                    # iteration index).
                    assert len(lambda_args) in (1, 2)
                    element_var = var_for_lambda_arg(lambda_args[0], 'item')
                    index_var = (
                        var_for_lambda_arg(lambda_args[1], 'index', T.Int)
                        if len(lambda_args) == 2
                        else None
                    )

                    # Finally lower the expressions
                    assert isinstance(call_expr.f_name, L.DotExpr)
                    coll_expr = lower(call_expr.f_name.f_prefix)
                    inner_expr = lower(lambda_expr.f_body)
                    result = E.Map.create_expanded(coll_expr, inner_expr,
                                                   element_var, index_var)
                    return result

                else:
                    # Otherwise, this call must be a method invocation. Note
                    # that not all methods map to actual field access in the
                    # generated code. For instance, calls to the String.join
                    # built-in method are turned into Join instances, so the
                    # "callee" variable below is not necessarily a FieldAccess
                    # instance.
                    callee = lower(expr.f_name)
                    args, kwargs = lower_args()
                    return callee(*args, **kwargs)

            elif isinstance(expr, L.CharLit):
                return E.CharacterLiteral(denoted_char_lit(expr))

            elif isinstance(expr, L.DotExpr):
                # Dotted expressions can designate an enum value or a member
                # access. Resolving the suffix determines how to process this.
                suffix_decl = check_referenced_decl(expr.f_suffix)

                if isinstance(suffix_decl, L.EnumLitDecl):
                    # The suffix refers to the declaration of en enum
                    # value: the prefix must designate the corresponding enum
                    # type.
                    enum_type_node = check_referenced_decl(expr.f_prefix)
                    assert isinstance(enum_type_node, L.EnumTypeDecl)
                    enum_type = self.lower_type_decl(enum_type_node)
                    assert isinstance(enum_type, EnumType)

                    name = names.Name.from_lower(expr.f_suffix.text)
                    return enum_type.values_dict[name].to_abstract_expr

                else:
                    # Otherwise, the prefix is a regular expression, so this
                    # dotted expression is an access to a member.
                    prefix = lower(expr.f_prefix)
                    assert isinstance(prefix, E.AbstractExpression)
                    return getattr(prefix, expr.f_suffix.text)

            elif isinstance(expr, L.IfExpr):
                # We want to turn the following pattern::
                #
                #   IfExpr(C1, E1, [(C2, E2), (C3, E3), ...], E_last)
                #
                # into the following expression tree::
                #
                #   If(C1, E1,
                #      If(C2, E2,
                #         If(C3, E3,
                #            ... E_Last)))
                #
                # so first translate the "else" expression (E_last), then
                # reverse iterate on the alternatives to wrap this expression
                # with the conditional checks.
                result = lower(expr.f_else_expr)
                conditions = [(alt.f_cond_expr, alt.f_then_expr)
                              for alt in expr.f_alternatives]
                conditions.append((expr.f_cond_expr, expr.f_then_expr))
                for c, e in reversed(conditions):
                    result = E.If(lower(c), lower(e), result)
                return result

            elif isinstance(expr, L.Isa):
                subexpr = lower(expr.f_expr)
                nodes = [
                    self.resolve_type_decl(type_ref.p_designated_type)
                    for type_ref in expr.f_dest_type
                ]
                return E.IsA(subexpr, *nodes)

            elif isinstance(expr, L.NotExpr):
                return E.Not(lower(expr.f_expr))

            elif isinstance(expr, L.NullLit):
                result_type = self.resolve_type_decl(expr.p_check_expr_type)
                return E.No(result_type)

            elif isinstance(expr, L.NumLit):
                return E.Literal(int(expr.text))

            elif isinstance(expr, L.ParenExpr):
                return lower(expr.f_expr)

            elif isinstance(expr, L.StringLit):
                return E.SymbolLiteral(denoted_string_lit(expr))

            elif isinstance(expr, L.RefId):
                decl = check_referenced_decl(expr)
                if isinstance(decl, L.NodeDecl):
                    return E.Self
                elif isinstance(decl, L.SelfDecl):
                    return E.Entity
                elif isinstance(decl, L.EnumLitDecl):
                    # TODO: handle all enum types
                    enum_type_decl = decl.p_get_type()
                    assert self.compiled_types.get(enum_type_decl) == T.Bool
                    assert decl.text in ('true', 'false')
                    return E.Literal(decl.text == 'true')
                else:
                    assert isinstance(decl, L.BaseValDecl)
                    return env[decl]

            else:
                assert False, 'Unhandled expression: {}'.format(expr)

        return lower(expr)

    def lower_property(self, full_decl: L.FullDecl) -> PropertyDef:
        """
        Lower the property described in ``decl``.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FunDecl)
        annotations = parse_annotations(self.ctx, FunAnnotations, full_decl)
        return_type = self.resolve_type_decl(
            decl.f_return_type.p_designated_type
        )

        env: LocalsEnv = {}

        # Lower arguments and register them in the environment
        args: List[Argument] = []
        for a in decl.f_args:
            if a.f_default_val is None:
                default_value = None
            else:
                default_value = self.lower_expr(a.f_default_val, env)
                default_value.prepare()

            arg = Argument(
                name=names.Name.from_lower(a.f_syn_name.text),
                type=self.resolve_type_decl(a.f_decl_type.p_designated_type),
                default_value=default_value
            )
            args.append(arg)
            env[a] = arg.var

        # Lower the expression itself
        if annotations.abstract:
            assert decl.f_body is None
            expr = None
        else:
            expr = self.lower_expr(decl.f_body, env)

        result = PropertyDef(
            expr=expr,
            prefix=AbstractNodeData.PREFIX_PROPERTY,
            doc=self.ctx.lkt_doc(full_decl),

            # When the @export annotation is missing, use "None" to mean
            # "public status unspecified", as the property can still be public
            # thanks to inheritance.
            public=annotations.export or None,

            abstract=annotations.abstract,
            type=return_type,
            abstract_runtime_check=False,
            dynamic_vars=None,
            memoized=False,
            call_memoizable=False,
            memoize_in_populate=False,
            external=False,
            uses_entity_info=None,
            uses_envs=None,
            optional_entity_info=False,
            warn_on_unused=True,
            ignore_warn_on_node=None,
            call_non_memoizable_because=None,
            activate_tracing=False,
            dump_ir=False
        )
        result.arguments.extend(args)

        return result

    def lower_fields(self,
                     decls: L.DeclBlock,
                     allowed_field_types: Tuple[Type[AbstractNodeData], ...]) \
            -> List[Tuple[names.Name, AbstractNodeData]]:
        """
        Lower the fields described in the given DeclBlock node.

        :param decls: Declarations to process.
        :param allowed_field_types: Set of types allowed for the fields to
        load.
        """
        result = []
        for full_decl in decls:
            decl = full_decl.f_decl

            # Check field name conformity
            name_text = decl.f_syn_name.text
            check_source_language(
                not name_text.startswith('_'),
                'Underscore-prefixed field names are not allowed'
            )
            check_source_language(
                name_text.lower() == name_text,
                'Field names must be lower-case'
            )
            name = names.Name.from_lower(name_text)

            field: AbstractNodeData

            if isinstance(decl, L.FunDecl):
                check_source_language(
                    any(issubclass(PropertyDef, cls)
                        for cls in allowed_field_types),
                    'Properties not allowed in this context'
                )
                field = self.lower_property(full_decl)
            else:
                field = self.lower_base_field(full_decl, allowed_field_types)

            field.location = Location.from_lkt_node(decl)
            result.append((name, cast(AbstractNodeData, field)))

        return result

    def create_node(self,
                    decl: L.BasicClassDecl,
                    annotations: BaseNodeAnnotations) -> ASTNodeType:
        """
        Create an ASTNodeType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        is_enum_node = isinstance(annotations, EnumNodeAnnotations)
        loc = Location.from_lkt_node(decl)

        # Resolve the base node (if any)
        base_type: Optional[ASTNodeType]

        # Root node case
        if decl.f_base_type is None:
            check_source_language(
                get_trait(decl, "Node") is not None,
                'The root node must implement the Node trait'
            )

            if CompiledTypeRepo.root_grammar_class is not None:
                check_source_language(
                    False,
                    'There can be only one root node ({})'.format(
                        CompiledTypeRepo.root_grammar_class.dsl_name
                    )
                )

            base_type = None
            is_token_node = is_error_node = False
        else:
            base_type_decl = decl.f_base_type.p_designated_type
            base_type = cast(ASTNodeType,
                             self.lower_type_decl(base_type_decl))

            # This is a token node if either the TokenNode trait is implemented
            # or if the base node is a token node itself. Likewise for
            # ErrorNode.
            is_token_node = get_trait(decl, "TokenNode") is not None
            is_error_node = get_trait(decl, "ErrorNode") is not None

            check_source_language(
                base_type is not base_type.is_enum_node,
                'Inheritting from an enum node is forbidden'
            )

        # Lower fields. Regular nodes can hold all types of fields, but token
        # nodes and enum nodes can hold only user field and properties.
        allowed_field_types = (
            (UserField, PropertyDef)
            if is_token_node or is_enum_node
            else (AbstractNodeData, )
        )
        fields = self.lower_fields(decl.f_decls, allowed_field_types)

        # For qualifier enum nodes, add the synthetic "as_bool" abstract
        # property that each alternative will override.
        is_bool_node = False
        if (
            isinstance(annotations, EnumNodeAnnotations)
            and annotations.qualifier
        ):
            prop = AbstractProperty(
                type=T.Bool, public=True,
                doc='Return whether this node is present'
            )
            prop.location = loc
            fields.append((names.Name('As_Bool'), prop))
            is_bool_node = True

        result = ASTNodeType(
            names.Name.from_camel(decl.f_syn_name.text),
            location=loc,
            doc=self.ctx.lkt_doc(decl.parent),
            base=base_type,
            fields=fields,
            is_abstract=(not isinstance(annotations, NodeAnnotations)
                         or annotations.abstract),
            is_token_node=is_token_node,
            is_error_node=is_error_node,
            has_abstract_list=annotations.has_abstract_list,
            is_enum_node=is_enum_node,
            is_bool_node=is_bool_node,
        )

        # Create alternatives for enum nodes
        if isinstance(annotations, EnumNodeAnnotations):
            assert isinstance(decl, L.EnumClassDecl)
            self.create_enum_node_alternatives(
                alternatives=sum(
                    (list(b.f_decls) for b in decl.f_branches), []
                ),
                enum_node=result,
                qualifier=annotations.qualifier
            )

        return result

    def create_enum_node_alternatives(
        self,
        alternatives: List[L.EnumClassAltDecl],
        enum_node: ASTNodeType,
        qualifier: bool
    ) -> None:
        """
        Create ASTNodeType instances for the alternatives of an enum node.

        :param alternatives: Declarations for the alternatives to lower.
        :param enum_node: Enum node that owns these alternatives.
        :param qualifier: Whether this enum node has the "@qualifier"
            annotation.
        """
        # RA22-015: initialize this to True for enum nodes directly in
        # ASTNodeType's constructor.
        enum_node.is_type_resolved = True

        enum_node._alternatives = []
        enum_node._alternatives_map = {}

        # All enum classes must have at least one alternative, except those
        # with the "@qualifier" annotation, which implies automatic
        # alternatives.
        if qualifier:
            check_source_language(
                not len(alternatives),
                'Enum nodes with @qualifier cannot have explicit alternatives'
            )
            alt_descriptions = [
                EnumNodeAlternative(names.Name(alt_name),
                                    enum_node,
                                    None,
                                    enum_node.location)
                for alt_name in ('Present', 'Absent')
            ]
        else:
            check_source_language(
                len(alternatives) > 0,
                'Missing alternatives for this enum node'
            )
            alt_descriptions = [
                EnumNodeAlternative(names.Name.from_camel(alt.f_syn_name.text),
                                    enum_node,
                                    None,
                                    Location.from_lkt_node(alt))
                for alt in alternatives
            ]

        # Now create the ASTNodeType instances themselves
        alt_nodes: List[ASTNodeType] = []
        for i, alt in enumerate(alt_descriptions):
            # Override the abstract "as_bool" property that all qualifier enum
            # nodes define.
            fields: List[Tuple[str, AbstractNodeData]] = []
            if qualifier:
                is_present = i == 0
                prop = Property(is_present)
                prop.location = enum_node.location
                fields.append(('as_bool', prop))

            alt.alt_node = ASTNodeType(
                name=alt.full_name, location=enum_node.location, doc='',
                base=enum_node,
                fields=fields,
                dsl_name='{}.{}'.format(enum_node.dsl_name,
                                        alt.base_name.camel)
            )
            alt_nodes.append(alt.alt_node)

        # Finally create enum node-local indexes to easily fetch the
        # ASTNodeType instances later on.
        enum_node._alternatives = alt_nodes
        enum_node._alternatives_map = {
            alt.base_name.camel: alt_node
            for alt, alt_node in zip(alt_descriptions, alt_nodes)
        }

    def create_enum(self,
                    decl: L.EnumTypeDecl,
                    annotations: EnumAnnotations) -> EnumType:
        """
        Create an EnumType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        # Decode the list of enum literals and validate them
        value_names = []
        for lit in decl.f_literals:
            name = lit.f_syn_name.text
            check_source_language(
                name not in value_names,
                'The "{}" literal is present twice'
            )
            value_names.append(name)

        return EnumType(
            name=names.Name.from_camel(decl.f_syn_name.text),
            location=Location.from_lkt_node(decl),
            doc=self.ctx.lkt_doc(decl.parent),
            value_names=[names.Name.from_lower(n) for n in value_names],
        )

    def create_struct(self,
                      decl: L.StructDecl,
                      annotations: StructAnnotations) -> StructType:
        """
        Create a StructType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        return StructType(
            name=names.Name.from_camel(decl.f_syn_name.text),
            location=Location.from_lkt_node(decl),
            doc=self.ctx.lkt_doc(decl.parent),
            fields=self.lower_fields(decl.f_decls, (UserField, )),
        )


def create_types(ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]) -> None:
    """
    Create types from Lktlang units.

    :param ctx: Context in which to create these types.
    :param lkt_units: Non-empty list of analysis units where to look for type
        declarations.
    """
    LktTypesLoader(ctx, lkt_units)
