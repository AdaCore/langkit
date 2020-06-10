"""
Module to gather the logic to lower Lkt syntax trees to Langkit internal data
structures.
"""

from collections import OrderedDict
import json
import os.path

from langkit.compiled_types import (ASTNodeType, AbstractNodeData,
                                    CompiledType, CompiledTypeRepo, Field, T,
                                    UserField)
from langkit.diagnostics import DiagnosticError, check_source_language
from langkit.expressions import PropertyDef
from langkit.lexer import (Alt, Case, Ignore, Lexer, LexerToken, Literal,
                           NoCaseLit, Pattern, TokenFamily, WithSymbol,
                           WithText, WithTrivia)
import langkit.names as names
from langkit.parsers import (Discard, DontSkip, Grammar, List, Null, Opt, Or,
                             Pick, Predicate, Skip, _Row, _Token, _Transform)

# RA22-015: in order to allow bootstrap, we need to import liblktlang only if
# we are about to process LKT grammar rules.


def pattern_as_str(str_lit):
    """
    Return the regexp string associated to this string literal node.

    :type str_lit: liblktlang.StringLit
    :rtype: str
    """
    return json.loads(str_lit.text[1:])


def parse_static_bool(ctx, expr):
    """
    Return the bool value that this expression denotes.

    :param liblktlang.Expr expr: Expression tree to parse.
    :rtype: bool
    """
    import liblktlang

    with ctx.lkt_context(expr):
        check_source_language(isinstance(expr, liblktlang.RefId)
                              and expr.text in ('false', 'true'),
                              'Boolean literal expected')

    return expr.text == 'true'


def load_lkt(lkt_file):
    """
    Load a Lktlang source file and return the closure of Lkt units referenced.
    Raise a DiagnosticError if there are parsing errors.

    :param str lkt_file: Name of the file to parse.
    :rtype: liblktlang.AnalysisUnit
    """
    import liblktlang

    units_map = OrderedDict()
    diagnostics = []

    def process_unit(unit):
        if unit.filename in units_map:
            return

        # Register this unit and its diagnostics
        units_map[unit.filename] = unit
        for d in unit.diagnostics:
            diagnostics.append((unit, d))

        # Recursively process the units it imports. In case of parsing error,
        # just stop the recursion: the collection of diagnostics is enough.
        if not unit.diagnostics:
            import_stmts = list(unit.root.f_imports)
            for imp in import_stmts:
                process_unit(imp.p_referenced_unit)

    # Load ``lkt_file`` and all the units it references, transitively
    process_unit(liblktlang.AnalysisContext().get_from_file(lkt_file))

    # If there are diagnostics, forward them to the user. TODO: hand them to
    # langkit.diagnostic.
    if diagnostics:
        for u, d in diagnostics:
            print('{}:{}'.format(os.path.basename(u.filename), d))
        raise DiagnosticError()
    return list(units_map.values())


def find_toplevel_decl(ctx, lkt_units, node_type, label):
    """
    Look for a top-level declaration of type ``node_type`` in the given units.

    If none or several are found, emit error diagnostics. Return the associated
    full declaration.

    :param list[liblktlang.AnalysisUnit] lkt_units: List of units where to
        look.
    :param langkit.compiled_types.ASTNode: Node type to look for.
    :param str label: Human readable string for what to look for. Used to
        create diagnostic mesages.
    :rtype: liblktlang.FullDecl
    """
    result = None
    for unit in lkt_units:
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
        check_source_language(result is not None, 'missing {}'.format(label))

    return result


class AnnotationSpec(object):
    """
    Synthetic description of how a declaration annotation works.
    """

    def __init__(self, name, unique, require_args, default_value=None):
        """
        :param str name: Name of the annotation (``foo`` for the ``@foo``
            annotation).
        :param bool unique: Whether this annotation can appear at most once for
            a given declaration.
        :param bool require_args: Whether this annotation requires arguments.
        :param default_value: For unique annotations, value to use in case the
            annotation is absent.
        """
        self.name = name
        self.unique = unique
        self.require_args = require_args
        self.default_value = default_value if unique else []

    def interpret(self, ctx, args, kwargs):
        """
        Subclasses must override this in order to interpret an annotation.

        This method must validate and interpret ``args`` and ``kwargs``, and
        return a value suitable for annotations processing.

        :param list[liblktlang.Expr] args: Positional arguments for the
            annotation.
        :param dict[str, liblktlang.Expr] kwargs: Keyword arguments for the
            annotation.
        """
        raise NotImplementedError

    def parse_single_annotation(self, ctx, result, annotation):
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
            value = self.interpret(ctx, (), {})
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


class ParsedAnnotations:
    """
    Namespace object to hold annotation parsed values.
    """
    pass


class FlagAnnotationSpec(AnnotationSpec):
    """
    Convenience subclass for flags.
    """
    def __init__(self, name):
        super(FlagAnnotationSpec, self).__init__(
            name, unique=True, require_args=False, default_value=False
        )

    def interpret(self, ctx, args, kwargs):
        return True


class SpacingAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @spacing annotations for lexer.
    """
    def __init__(self):
        super(SpacingAnnotationSpec, self).__init__('spacing', unique=False,
                                                    require_args=True)

    def interpret(self, ctx, args, kwargs):
        import liblktlang

        check_source_language(len(args) == 2 and not kwargs,
                              'Exactly two positional arguments expected')
        couple = []
        for f in args:
            # Check that we only have RefId nodes, but do not attempt to
            # translate them to TokenFamily instances: at the point we
            # interpret annotations, the set of token families is not ready
            # yet.
            check_source_language(isinstance(f, liblktlang.RefId),
                                  'Token family name expected')
            couple.append(f)
        return tuple(couple)


class TokenAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @text/symbol/trivia annotations for tokens.
    """
    def __init__(self, name):
        super(TokenAnnotationSpec, self).__init__(name, unique=True,
                                                  require_args=True)

    def interpret(self, ctx, args, kwargs):
        check_source_language(not args, 'No positional argument allowed')

        try:
            start_ignore_layout = kwargs.pop('start_ignore_layout')
        except KeyError:
            start_ignore_layout = False
        else:
            start_ignore_layout = parse_static_bool(ctx, start_ignore_layout)

        try:
            end_ignore_layout = kwargs.pop('end_ignore_layout')
        except KeyError:
            end_ignore_layout = False
        else:
            end_ignore_layout = parse_static_bool(ctx, end_ignore_layout)

        check_source_language(
            not kwargs,
            'Invalid arguments: {}'.format(', '.join(sorted(kwargs)))
        )

        return (start_ignore_layout, end_ignore_layout)


# Annotation specs to use when no annotation is allowed
no_annotations = []

# Annotation specs for grammar rules
grammar_rule_annotations = [FlagAnnotationSpec('main_rule')]

# Annotation specs for lexers
lexer_annotations = [SpacingAnnotationSpec(),
                     FlagAnnotationSpec('track_indent')]
token_annotations = [TokenAnnotationSpec('text'),
                     TokenAnnotationSpec('trivia'),
                     TokenAnnotationSpec('symbol'),
                     FlagAnnotationSpec('newline_after'),
                     FlagAnnotationSpec('pre_rule'),
                     FlagAnnotationSpec('ignore')]
token_cls_map = {'text': WithText,
                 'trivia': WithTrivia,
                 'symbol': WithSymbol}

# Annotations for node declarations
node_annotations = [FlagAnnotationSpec('abstract'),
                    FlagAnnotationSpec('has_abstract_list'),
                    FlagAnnotationSpec('root_node'),
                    FlagAnnotationSpec('token_node')]
field_annotations = [FlagAnnotationSpec('abstract'),
                     FlagAnnotationSpec('null_field'),
                     FlagAnnotationSpec('parse_field')]


def parse_annotations(ctx, specs, full_decl):
    """
    Parse annotations according to the given specs. Return a dict that
    contains the intprereted annotation values for each present annotation.

    :param list[AnnotationSpec] specs: Annotation specifications for
        allowed annotations.
    :param liblktlang.FullDecl full_decl: Declaration whose annotations are to
        be parsed.
    :rtype: dict[str, object]
    """
    annotations = list(full_decl.f_decl_annotations)

    # If no annotations are allowed, just check there are none
    if not specs:
        check_source_language(len(annotations) == 0, 'no annotation allowed')
        return {}

    # Build a mapping for all specs
    specs_map = {}
    for s in specs:
        assert s.name not in specs_map
        specs_map[s.name] = s

    # Process annotations
    values = {}
    for a in annotations:
        name = a.f_name.text
        spec = specs_map.get(name, None)
        with ctx.lkt_context(a):
            check_source_language(
                spec is not None,
                'Invalid annotation: {}'.format(name)
            )
            spec.parse_single_annotation(ctx, values, a)


    # Use the default value for absent annotations
    for s in specs:
        values.setdefault(s.name, s.default_value)

    # Create the namespace object to hold results
    result = ParsedAnnotations()
    for k, v in values.items():
        setattr(result, k, v)
    return result


def create_lexer(ctx, lkt_units):
    """
    Create and populate a lexer from a Lktlang unit.

    :param list[liblktlang.AnalysisUnit] lkt_units: Non-empty list of analysis
        units where to look for the grammar.
    :rtype: langkit.lexer.Lexer
    """
    import liblktlang

    # Look for the LexerDecl node in top-level lists
    full_lexer = find_toplevel_decl(ctx, lkt_units, liblktlang.LexerDecl,
                                    'lexer')
    with ctx.lkt_context(full_lexer):
        lexer_annot = parse_annotations(ctx, lexer_annotations, full_lexer)

    patterns = {}
    """
    Mapping from pattern names to the corresponding regular expression.

    :type: dict[names.Name, str]
    """

    token_family_sets = {}
    """
    Mapping from token family names to the corresponding sets of tokens that
    belong to this family.

    :type: dict[names.Name, Token]
    """

    token_families = {}
    """
    Mapping from token family names to the corresponding token families.  We
    build this late, once we know all tokens and all families.

    :type: dict[names.Name, TokenFamily]
    """

    tokens = {}
    """
    Mapping from token names to the corresponding tokens.

    :type: dict[names.Name, Token]
    """

    rules = []
    pre_rules = []
    """
    Lists of regular and pre lexing rules for this lexer.

    :type: list[(langkit.lexer.Matcher, langkit.lexer.Action)]
    """

    newline_after = []
    """
    List of tokens after which we must introduce a newline during unparsing.

    :type: list[Token]
    """

    def ignore_constructor(start_ignore_layout, end_ignore_layout):
        """
        Adapter to build a Ignore instance with the same API as WithText
        constructors.
        """
        del start_ignore_layout, end_ignore_layout
        return Ignore()

    def process_family(f):
        """
        Process a LexerFamilyDecl node. Register the token family and process
        the rules it contains.

        :type f: liblktlang.LexerFamilyDecl
        """
        with ctx.lkt_context(f):
            # Create the token family, if needed
            name = names.Name.from_lower(f.f_syn_name.text)
            token_set = token_family_sets.setdefault(name, set())

            for r in f.f_rules:
                check_source_language(
                    isinstance(r.f_decl, liblktlang.GrammarRuleDecl),
                    'Only lexer rules allowed in family blocks'
                )
                process_token_rule(r, token_set)

    def process_token_rule(r, token_set=None):
        """
        Process the full declaration of a GrammarRuleDecl node: create the
        token it declares and lower the optional associated lexing rule.

        :param liblktlang.FullDecl r: Full declaration for the GrammarRuleDecl
            to process.
        :param None|set[TokenAction] token_set: If this declaration appears in
            the context of a token family, this adds the new token to this set.
            Must be left to None otherwise.
        """
        with ctx.lkt_context(r):
            rule_annot = parse_annotations(ctx, token_annotations, r)

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
            if token_set is not None:
                token_set.add(token)
            if rule_annot.newline_after:
                newline_after.append(token)

            # Lower the lexing rule, if present
            matcher_expr = r.f_decl.f_expr
            if matcher_expr is not None:
                rule = (lower_matcher(matcher_expr), token)
                if is_pre:
                    pre_rules.append(rule)
                else:
                    rules.append(rule)

    def process_pattern(full_decl):
        """
        Process a pattern declaration.

        :param liblktlang.FullDecl r: Full declaration for the ValDecl to
            process.
        """
        parse_annotations(ctx, [], full_decl)
        decl = full_decl.f_decl
        lower_name = decl.f_syn_name.text
        name = names.Name.from_lower(lower_name)

        with ctx.lkt_context(decl):
            check_source_language(name not in patterns,
                                  'Duplicate pattern name')
            check_source_language(decl.f_decl_type is None,
                                  'Patterns must have automatic types in'
                                  ' lexers')
            check_source_language(
                isinstance(decl.f_val, liblktlang.StringLit)
                and decl.f_val.p_is_regexp_literal,
                'Pattern string literal expected'
            )
            # TODO: use StringLit.p_denoted_value when properly implemented
            patterns[name] = pattern_as_str(decl.f_val)

    def lower_matcher(expr):
        """
        Lower a token matcher to our internals.

        :type expr: liblktlang.GrammarExpr
        :rtype: langkit.lexer.Matcher
        """
        with ctx.lkt_context(expr):
            if isinstance(expr, liblktlang.TokenLit):
                return Literal(json.loads(expr.text))
            elif isinstance(expr, liblktlang.TokenNoCaseLit):
                return NoCaseLit(json.loads(expr.text))
            elif isinstance(expr, liblktlang.TokenPatternLit):
                return Pattern(pattern_as_str(expr))
            else:
                check_source_language(False, 'Invalid lexing expression')

    def lower_token_ref(ref):
        """
        Return the Token that `ref` refers to.

        :type ref: liblktlang.RefId
        :rtype: Token
        """
        with ctx.lkt_context(ref):
            token_name = names.Name.from_lower(ref.text)
            check_source_language(token_name in tokens,
                                  'Unknown token: {}'.format(token_name.lower))
            return tokens[token_name]

    def lower_family_ref(ref):
        """
        Return the TokenFamily that `ref` refers to.

        :type ref: liblktlang.RefId
        :rtype: TokenFamily
        """
        with ctx.lkt_context(ref):
            name_lower = ref.text
            name = names.Name.from_lower(name_lower)
            check_source_language(
                name in token_families,
                'Unknown token family: {}'.format(name_lower)
            )
            return token_families[name]

    def lower_case_alt(alt):
        """
        Lower the alternative of a case lexing rule.

        :type alt: liblktlang.BaseLexerCaseRuleAlt
        :rtype: Alt
        """
        prev_token_cond = None
        if isinstance(alt, liblktlang.LexerCaseRuleCondAlt):
            prev_token_cond = [lower_token_ref(ref)
                               for ref in alt.f_cond_exprs]
        return Alt(prev_token_cond=prev_token_cond,
                   send=lower_token_ref(alt.f_send.f_sent),
                   match_size=int(alt.f_send.f_match_size.text))

    # Go through all rules to register tokens, their token families and lexing
    # rules.
    for full_decl in full_lexer.f_decl.f_rules:
        with ctx.lkt_context(full_decl):
            if isinstance(full_decl, liblktlang.LexerFamilyDecl):
                # This is a family block: go through all declarations inside it
                process_family(full_decl)

            elif isinstance(full_decl, liblktlang.FullDecl):
                # There can be various types of declarations in lexers...
                decl = full_decl.f_decl

                if isinstance(decl, liblktlang.GrammarRuleDecl):
                    # Here, we have a token declaration, potentially associated
                    # with a lexing rule.
                    process_token_rule(full_decl)

                elif isinstance(decl, liblktlang.ValDecl):
                    # This is the declaration of a pattern
                    process_pattern(full_decl)

                else:
                    check_source_language(False,
                                          'Unexpected declaration in lexer')

            elif isinstance(full_decl, liblktlang.LexerCaseRule):
                syn_alts = list(full_decl.f_alts)

                # This is a rule for conditional lexing: lower its matcher and
                # its alternative rules.
                matcher = lower_matcher(full_decl.f_expr)
                check_source_language(
                    len(syn_alts) == 2 and
                    isinstance(syn_alts[0],
                               liblktlang.LexerCaseRuleCondAlt) and
                    isinstance(syn_alts[1],
                               liblktlang.LexerCaseRuleDefaultAlt),
                    'Invalid case rule topology'
                )
                rules.append(Case(matcher,
                                  lower_case_alt(syn_alts[0]),
                                  lower_case_alt(syn_alts[1])))

            else:
                # The grammar should make the following dead code
                assert False, 'Invalid lexer rule: {}'.format(full_decl)

    # Create the LexerToken subclass to define all tokens and token families
    items = {}
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
    for tf1, tf2 in lexer_annot.spacing:
        result.add_spacing((lower_family_ref(tf1),
                            lower_family_ref(tf2)))
    result.add_newline_after(*newline_after)

    return result


def create_grammar(ctx, lkt_units):
    """
    Create a grammar from a set of Lktlang units.

    Note that this only initializes a grammar and fetches relevant declarations
    in the Lktlang unit. The actual lowering on grammar rules happens in a
    separate pass: see lower_all_lkt_rules.

    :param list[liblktlang.AnalysisUnit] lkt_units: Non-empty list of analysis
        units where to look for the grammar.
    :rtype: langkit.parsers.Grammar
    """
    import liblktlang

    # Look for the GrammarDecl node in top-level lists
    full_grammar = find_toplevel_decl(ctx, lkt_units, liblktlang.GrammarDecl,
                                      'grammar')

    # No annotation allowed for grammars
    with ctx.lkt_context(full_grammar):
        parse_annotations(ctx, [], full_grammar)

    # Get the list of grammar rules. This is where we check that we only have
    # grammar rules, that their names are unique, and that they have valid
    # annotations.
    all_rules = OrderedDict()
    main_rule_name = None
    for full_rule in full_grammar.f_decl.f_rules:
        with ctx.lkt_context(full_rule):
            r = full_rule.f_decl

            # Make sure we have a grammar rule
            check_source_language(isinstance(r, liblktlang.GrammarRuleDecl),
                                  'grammar rule expected')
            rule_name = r.f_syn_name.text

            # Register the main rule if the appropriate annotation is present
            a = parse_annotations(ctx, grammar_rule_annotations, full_rule)
            if a.main_rule:
                check_source_language(main_rule_name is None,
                                      'only one main rule allowed')
                main_rule_name = rule_name

            all_rules[rule_name] = r.f_expr

    # Now create the result grammar. We need exactly one main rule for that.
    with ctx.lkt_context(full_grammar):
        check_source_language(main_rule_name is not None,
                              'Missing main rule (@main_rule annotation)')
    result = Grammar(main_rule_name, ctx.lkt_loc(full_grammar))

    # Translate rules (all_rules) later, as node types are not available yet
    result._all_lkt_rules.update(all_rules)
    return result


def lower_grammar_rules(ctx):
    """
    Translate syntactic Lkt rules to Parser objects.
    """
    grammar = ctx.grammar
    if not grammar._all_lkt_rules:
        return
    import liblktlang

    # Build a mapping for all tokens registered in the lexer. Use lower case
    # names, as this is what the concrete syntax is supposed to use.
    tokens = {token.name.lower: token
              for token in ctx.lexer.tokens.tokens}

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

    def denoted_string_literal(string_lit):
        return eval(string_lit.text)

    def resolve_node_ref(node_ref):
        """
        Helper to resolve a node name to the actual AST node.

        :param liblktlang.RefID node_ref: Node that is the reference to the
            AST node.
        :rtype: ASTNodeType
        """
        # For convenience, accept null input nodes, as we generally want to
        # forward them as-is to the lower level parsing machinery.
        if node_ref is None:
            return None

        elif isinstance(node_ref, liblktlang.DotExpr):
            # Get the altenatives mapping for the prefix_node enum node
            prefix_node = resolve_node_ref(node_ref.f_prefix)
            with ctx.lkt_context(prefix_node):
                try:
                    alt_map = enum_nodes[prefix_node]
                except KeyError:
                    check_source_language(
                        False,
                        'Non-qualifier enum node expected (got {})'
                        .format(prefix_node.dsl_name)
                    )

            # Then resolve the alternative
            suffix = node_ref.f_suffix.text
            with ctx.lkt_context(node_ref.f_suffix):
                try:
                    return alt_map[suffix]
                except KeyError:
                    check_source_language(
                        False,
                        'Unknown enum node alternative: {}'.format(suffix)
                    )

        elif isinstance(node_ref, liblktlang.GenericTypeRef):
            check_source_language(
                node_ref.f_type_name.text == u'ASTList',
                'Bad generic type name: only ASTList is valid in this context'
            )

            params = node_ref.f_params
            check_source_language(
                len(params) == 1,
                '1 type argument expected, got {}'.format(len(params))
            )
            return resolve_node_ref(params[0]).list

        elif isinstance(node_ref, liblktlang.SimpleTypeRef):
            return resolve_node_ref(node_ref.f_type_name)

        else:
            assert isinstance(node_ref, liblktlang.RefId)
            with ctx.lkt_context(node_ref):
                node_name = node_ref.text
                try:
                    return nodes[node_name]
                except KeyError:
                    check_source_language(False,
                                          'Unknown node: {}'.format(node_name))

    def lower(rule):
        """
        Helper to lower one parser.

        :param liblktlang.GrammarExpr rule: Grammar rule to lower.
        :rtype: Parser
        """
        # For convenience, accept null input rules, as we generally want to
        # forward them as-is to the lower level parsing machinery.
        if rule is None:
            return None

        loc = ctx.lkt_loc(rule)
        with ctx.lkt_context(rule):
            if isinstance(rule, liblktlang.ParseNodeExpr):
                node = resolve_node_ref(rule.f_node_name)

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

            elif isinstance(rule, liblktlang.GrammarToken):
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
                    assert isinstance(rule.f_expr, liblktlang.TokenLit)
                    match_text = denoted_string_literal(rule.f_expr)

                return _Token(val=val, match_text=match_text, location=loc)

            elif isinstance(rule, liblktlang.TokenLit):
                return _Token(denoted_string_literal(rule), location=loc)

            elif isinstance(rule, liblktlang.GrammarList):
                return List(lower(rule.f_expr),
                            empty_valid=rule.f_kind.text == '*',
                            list_cls=resolve_node_ref(rule.f_list_type),
                            sep=lower(rule.f_sep),
                            location=loc)

            elif isinstance(rule, (liblktlang.GrammarImplicitPick,
                                   liblktlang.GrammarPick)):
                return Pick(*[lower(subparser) for subparser in rule.f_exprs],
                            location=loc)

            elif isinstance(rule, liblktlang.GrammarRuleRef):
                return getattr(grammar, rule.f_node_name.text)

            elif isinstance(rule, liblktlang.GrammarOrExpr):
                return Or(*[lower(subparser)
                            for subparser in rule.f_sub_exprs],
                          location=loc)

            elif isinstance(rule, liblktlang.GrammarOpt):
                return Opt(lower(rule.f_expr), location=loc)

            elif isinstance(rule, liblktlang.GrammarOptGroup):
                return Opt(*[lower(subparser) for subparser in rule.f_expr],
                           location=loc)

            elif isinstance(rule, liblktlang.GrammarExprList):
                return Pick(*[lower(subparser) for subparser in rule],
                            location=loc)

            elif isinstance(rule, liblktlang.GrammarDiscard):
                return Discard(lower(rule.f_expr), location=loc)

            elif isinstance(rule, liblktlang.GrammarNull):
                return Null(resolve_node_ref(rule.f_name), location=loc)

            elif isinstance(rule, liblktlang.GrammarSkip):
                return Skip(resolve_node_ref(rule.f_name), location=loc)

            elif isinstance(rule, liblktlang.GrammarDontSkip):
                return DontSkip(lower(rule.f_expr),
                                lower(rule.f_dont_skip),
                                location=loc)

            elif isinstance(rule, liblktlang.GrammarPredicate):
                check_source_language(
                    isinstance(rule.f_prop_ref, liblktlang.DotExpr),
                    'Invalid property reference'
                )
                node = resolve_node_ref(rule.f_prop_ref.f_prefix)
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


def create_types(ctx, lkt_units):
    """
    Create types from Lktlang units.

    :param list[liblktlang.AnalysisUnit] lkt_units: Non-empty list of analysis
        units where to look for type declarations.
    """
    import liblktlang

    # Go through all units, build a map for all type definitions, indexed by
    # Name. This first pass allows the check of unique names.
    syntax_types = {}
    for unit in lkt_units:
        for full_decl in unit.root.f_decls:
            if not isinstance(full_decl.f_decl, liblktlang.TypeDecl):
                continue
            name_str = full_decl.f_decl.f_syn_name.text
            name = names.Name.from_camel(name_str)
            check_source_language(
                name not in syntax_types,
                'Duplicate type name: {}'.format(name_str)
            )
            syntax_types[name] = full_decl

    # Now create CompiledType instances for each type. To properly handle
    # node derivation, recurse on bases first and reject inheritance loops.

    # Map indexed by type Name. Unvisited types are absent, fully processed
    # types have an entry with the corresponding CompiledType, and currently
    # processed types have an entry associated with None.
    compiled_types = {}

    def resolve_type_ref(ref, defer):
        """
        Fetch the CompiledType instance corresponding to the given type
        reference.

        :param liblktlang.TypeRef ref: Type reference to resolve.
        :param bool defer: If True and this type is not lowered yet, return a
            TypeRepo.Defer instance. Lower the type if necessary in all other
            cases.
        :rtype: CompiledType|TypeRepo.Defer
        """
        with ctx.lkt_context(ref):
            if isinstance(ref, liblktlang.SimpleTypeRef):
                return create_type_from_name(
                    names.Name.from_camel(ref.f_type_name.text),
                    defer
                )

            elif isinstance(ref, liblktlang.GenericTypeRef):
                check_source_language(
                    isinstance(ref.f_type_name, liblktlang.RefId),
                    'Invalid generic type'
                )
                gen_type = ref.f_type_name.text
                gen_args = list(ref.f_params)
                if gen_type == 'ASTList':
                    check_source_language(
                        len(gen_args) == 1,
                        'Exactly one type argument expected'
                    )
                    elt_type = resolve_type_ref(gen_args[0], defer)
                    return elt_type.list

                else:
                    check_source_language(False, 'Unknown generic type')

            else:
                raise NotImplementedError(
                    'Unhandled type reference: {}'.format(ref)
                )

    def create_type_from_name(name, defer):
        """
        Fetch the CompiledType instance corresponding to the given type
        reference.

        :param names.Name name: Name of the type to create.
        :param bool defer: If True and this type is not lowered yet, return a
            TypeRepo.Defer instance. Lower the type if necessary in all other
            cases.
        :rtype: CompiledType|TypeRepo.Defer
        """
        full_decl = syntax_types.get(name)
        check_source_language(
            full_decl is not None,
            'Invalid type name: {}'.format(name.camel)
        )
        if defer:
            return getattr(T, name.camel)
        decl = full_decl.f_decl

        # Directly return already created CompiledType instances and raise an
        # error for cycles in the type inheritance graph.
        compiled_type = compiled_types.get(name, "not-visited")
        if isinstance(compiled_type, CompiledType):
            return compiled_type
        with ctx.lkt_context(decl):
            check_source_language(
                compiled_type is not None,
                'Type inheritance loop detected'
            )
            compiled_types[name] = None

            # Dispatch now to the appropriate type creation helper
            if isinstance(decl, liblktlang.ClassDecl):
                specs = node_annotations
                creator = create_node

            else:
                raise NotImplementedError(
                    'Unhandled type declaration: {}'.format(decl)
                )

            annotations = parse_annotations(ctx, specs, full_decl)
            result = creator(name, decl, annotations)
            compiled_types[name] = result
            return result

    def lower_fields(decls, allowed_field_types):
        """
        Lower the fields described in the given DeclBlock node.

        :param liblktlang.DeclBlock decls: Declarations to process.
        :param tuple[type] allowed_field_types: Set of types allowed for the
            fields to load.
        :rtype: list[(names.Name, AbstractNodeData)]
        """
        result = []
        for full_decl in decls:
            decl = full_decl.f_decl
            annotations = parse_annotations(ctx, field_annotations, full_decl)
            field_type = resolve_type_ref(decl.f_decl_type, defer=True)

            # Check field name conformity
            name = decl.f_syn_name.text
            check_source_language(
                not name.startswith('_'),
                'Underscore-prefixed field names are not allowed'
            )
            check_source_language(
                name.lower() == name,
                'Field names must be lower-case'
            )
            name = names.Name.from_lower(name)

            if decl.f_default_val:
                raise NotImplementedError(
                    'Field default values not handled yet'
                )

            if annotations.parse_field:
                cls = Field
                kwargs = {'abstract': annotations.abstract,
                          'null': annotations.null_field}
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
                kwargs = {}

            check_source_language(
                issubclass(cls, allowed_field_types),
                'Invalid field type in this context'
            )

            field = cls(type=field_type, doc=ctx.lkt_doc(full_decl), **kwargs)
            field.location = ctx.lkt_loc(decl)
            result.append((name, field))
        return result

    def create_node(name, decl, annotations):
        """
        Create an ASTNodeType instance.

        :param names.Name name: DSL name for this node type.
        :param liblktlang.ClassDecl decl: Corresponding declaration node.
        :param ParsedAnnotations annotations: Annotations for this declaration.
        :rtype: ASTNodeType
        """
        # Resolve the base node (if any)
        base = (resolve_type_ref(decl.f_base_type, defer=False)
                if decl.f_base_type else None)

        # Make sure the root_node annotation is used when appropriate
        root_node = annotations.root_node
        if base is None:
            check_source_language(
                root_node,
                'The root node requires a @root_node annotation'
            )
            if CompiledTypeRepo.root_grammar_class is not None:
                check_source_language(
                    False,
                    'There can be only one root node ({})'.format(
                        CompiledTypeRepo.root_grammar_class.dsl_name
                    )
                )
        else:
            check_source_language(
                not root_node,
                'Only the root node can have the @root_node annotation'
            )

        check_source_language(not len(decl.f_traits),
                              'Nodes cannot use traits')

        # This is a token node if either the annotation is present, or if the
        # base node is a token node itself.
        is_token_node = (
            annotations.token_node
            or (base is not None and base.is_token_node)
        )

        # Lower fields. Regular nodes can hold all types of fields, but token
        # nodes can hold only user field and properties.
        allowed_field_types = (
            (UserField, PropertyDef)
            if is_token_node
            else (AbstractNodeData, )
        )
        fields = lower_fields(decl.f_decls, allowed_field_types)

        return ASTNodeType(
            name,
            location=ctx.lkt_loc(decl),
            doc=ctx.lkt_doc(full_decl),
            base=base,
            fields=fields,
            is_abstract=annotations.abstract,
            is_token_node=is_token_node,
            has_abstract_list=annotations.has_abstract_list,
        )

    for name in sorted(syntax_types):
        create_type_from_name(name, defer=False)
