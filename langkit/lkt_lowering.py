"""
Module to gather the logic to lower Lkt syntax trees to Langkit internal data
structures.
"""

from __future__ import absolute_import, division, print_function

from collections import OrderedDict
import os.path

from langkit.diagnostics import DiagnosticError, check_source_language
from langkit.dsl import _ASTNodeMetaclass
from langkit.parsers import (Discard, DontSkip, Grammar, List, Null, Opt, Or,
                             Pick, Predicate, Skip, _Row, _Token, _Transform)

# RA22-015: in order to allow bootstrap, we need to import liblktlang only if
# we are about to process LKT grammar rules.


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
        diagnostics.extend(unit.diagnostics)

        # Recursively process the units it imports. In case of parsing error,
        # just stop the recursion: the collection of diagnostics is enough.
        if not unit.diagnostics:
            import_stmts = list(unit.root.f_imports)
            for imp in import_stmts:
                process_unit(imp.p_referenced_unit)

    # Load ``lkt_file`` and all the units it references, transitively
    basename = os.path.basename(lkt_file)
    process_unit(liblktlang.AnalysisContext().get_from_file(lkt_file))

    # If there are diagnostics, forward them to the user. TODO: hand them to
    # langkit.diagnostic.
    if diagnostics:
        for d in diagnostics:
            print('{}:{}'.format(basename, d))
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


def annotations(ctx, decl):
    """
    Build a Python dict for ``decl``'s annotations.

    :param liblktlang.FullDecl decl: Declaration whose annotations must be
        processed.
    :rtype: dict[str, liblktlang.LKNode]
    """
    result = {}
    for a in decl.f_decl_annotations:
        name = a.f_name.text
        with ctx.lkt_context(a):
            check_source_language(name not in result,
                                  'invalid double annotation')
        result[name] = (a, a.f_params)
    return result


def create_grammar(ctx, lkt_units):
    """
    Create a grammar from a set of Lktlang units.

    Note that this only initializes a grammar and fetches relevant declarations
    in the Lktlang unit. The actual lowering on grammar rules happens in a
    separate pass: see lower_all_lkt_rules.

    :param list[liblktlang.AnalysisUnit] lkt_unit: Non-empty list of analysis
        units where to look for the grammar.
    :rtype: langkit.parsers.Grammar
    """
    import liblktlang

    # Look for the GrammarDecl node in top-level lists
    full_grammar = find_toplevel_decl(ctx, lkt_units, liblktlang.GrammarDecl,
                                      'grammar')

    # No annotation allowed for grammars
    with ctx.lkt_context(full_grammar):
        check_source_language(not annotations(ctx, full_grammar),
                              'no annotation allowed')

    # Get the list of grammar rules. This is where we check that we only have
    # grammar rules, that their names are unique, and that they have valid
    # annotations.
    all_rules = OrderedDict()
    main_rule_name = None
    for full_rule in full_grammar.f_decl.f_rules:
        a = annotations(ctx, full_rule)

        # Make sure we have a grammar rule
        with ctx.lkt_context(full_rule):
            r = full_rule.f_decl
            check_source_language(isinstance(r, liblktlang.GrammarRuleDecl),
                                  'grammar rule expected')

            # TODO: remove the encoding once we support Python3. For now, the
            # rest of the grammar machinery expects byte strings.
            rule_name = r.f_syn_name.text.encode('ascii')

        # Detect and validate the only allowed annotation
        try:
            annot, params = a.pop('main_rule')
        except KeyError:
            pass
        else:
            with ctx.lkt_context(annot):
                check_source_language(
                    len(params) == 0,
                    'no parameters allowed for this main_rule annotation'
                )
                check_source_language(main_rule_name is None,
                                      'only one main rule allowed')
                main_rule_name = rule_name

        # Reject other annotations
        with ctx.lkt_context(full_rule):
            remaining = sorted(a)
            check_source_language(
                not remaining,
                'invalid annotations: {}'.format(', '.join(remaining))
            )

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
    nodes = {n._type.raw_name.camel: n
             for n in _ASTNodeMetaclass.astnode_types}

    # For every non-qualifier enum node, build a mapping from value names
    # (camel cased) to the corresponding enum node subclass.
    enum_nodes = {
        node: {alt.name.camel: alt for alt in node._alternatives}
        for node in nodes.values()
        if node._type.is_enum_node and not node._type.is_bool_node
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
                if node._type.is_bool_node:
                    return Opt(*subparsers, location=loc).as_bool(node)

                # Likewise for enum nodes
                elif node._type.base and node._type.base.is_enum_node:
                    return _Transform(_Row(*subparsers, location=loc),
                                      node.type_ref,
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
                    prop = getattr(node, prop_name)
                except AttributeError:
                    check_source_language(
                        False,
                        '{} has no {} property'
                        .format(node._name.camel_with_underscores,
                                prop_name)
                    )
                return Predicate(lower(rule.f_expr), prop, location=loc)

            else:
                raise NotImplementedError('unhandled parser: {}'.format(rule))

    for name, rule in grammar._all_lkt_rules.items():
        grammar._add_rule(name, lower(rule))
