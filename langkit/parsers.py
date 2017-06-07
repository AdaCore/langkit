"""
This file contains the base logic for the parser generator. It declares a base
class, Parser, from which every parsing primitive derives.

It contains both the public interface to the parsers (eg. the Rows, Opt, Or,
etc.. classes), and the engine implementation that will actually emit the final
code.

The way the code is generated is by recursively visiting the parser structure
and emitting the code corresponding to the declared parser. For example, for
the following rule definition::

    if_stmt=Row("if", G.expression, "then", G.statements, "endif")

The row structure will be visited recursively, emitting the corresponding code.

The parser generator generates separate functions for every rule that is
declared. It means that in the case of the previous example, a rule will be
declared for if_stmt, and for the `expression` and `statements` rule, that are
not defined in the example, but relied on explicitly.
"""

from __future__ import absolute_import, division, print_function

from collections import defaultdict
from contextlib import contextmanager
import difflib
from funcy import keep, split
import inspect

from langkit import compiled_types, names
from langkit.common import gen_name
from langkit.compile_context import get_context
from langkit.compiled_types import ASTNodeType, BoolType, CompiledType, Token
from langkit.diagnostics import (
    Context, Location, check_source_language, extract_library_location,
    Severity, WarningSet
)
from langkit.lexer import WithSymbol
from langkit.template_utils import TemplateEnvironment
from langkit.utils import (
    assert_type, common_ancestor, copy_with, type_check_instance, Log, is_same
)


def var_context():
    """
    Returns the var context for the current parser.

    :rtype: VarContext
    """
    return get_context().parsers_varcontext_stack[-1]


@contextmanager
def add_var_context():
    """
    Context manager that will push a variable context into the stack when
    compiling a parser.
    """
    vc = []
    get_context().parsers_varcontext_stack.append(vc)
    yield vc
    get_context().parsers_varcontext_stack.pop()


class VarDef(object):
    """
    Holder object for a variable in parsers. Creating an instance of vardef
    requires a context to exist already.

    If create is True, the variable will be automatically added to the current
    variable context when created, and an unique name will be generated. If
    create is False, it is considered that the definition is done in the
    template, so the name used is the name passed, and the variable won't be
    added to the context.
    """

    def __init__(self, base_name, type, create=True):
        self.type = type

        # Add this variable to the current var context
        if create:
            self.name = gen_name(base_name)
            var_context().append(self)
        else:
            self.name = names.Name.from_lower(base_name)

    def __getitem__(self, i):
        """
        Helper, will allow users to destructures var defs into name and type,
        like::

            name, type = var

        Which in turns will allow iteration on the var context like::

            for name, type in var_context:
                ...
        """
        return [self.name, self.type][i]

    def __unicode__(self):
        """
        Helper, expand var defs to their name when converted to strings,
        allows easy use of VarDef instances in code generation.
        """
        return unicode(self.name)


class GeneratedParser(object):
    """
    Simple holder for generated parsers.
    """

    def __init__(self, name, spec, body):
        self.name = name
        self.spec = spec
        self.body = body


def render(*args, **kwargs):
    return compiled_types.make_renderer().update({
        'is_tok':       type_check_instance(Tok),
        'is_row':       type_check_instance(Row),
        'is_defer':     type_check_instance(Defer),
        'is_transform': type_check_instance(Transform),
        'is_list':      type_check_instance(List),
        'is_opt':       type_check_instance(Opt),
        'is_null':      type_check_instance(Null),
        'is_extract':   type_check_instance(Extract),
        'is_class':     inspect.isclass,
        'ctx':          get_context(),
        'creates_node': creates_node,
    }).render(*args, **kwargs)


def resolve(parser):
    """
    :type parser: Parser|types.Token|ParserContainer
    :rtype: Parser
    """
    if isinstance(parser, Parser):
        return parser
    elif isinstance(parser, Token):
        return Tok(parser)
    elif isinstance(parser, basestring):
        return Tok(parser)
    else:
        raise Exception("Cannot resolve parser {}".format(parser))


class Grammar(object):
    """
    Holder for parsing rules.

    Parsing rules can be added incrementally while referencing each other: this
    class will automatically resolve forward references when needed.
    """

    def __init__(self, main_rule_name):
        self.rules = {}
        self.main_rule_name = main_rule_name
        self.location = extract_library_location()

    def context(self):
        return Context("In definition of grammar", self.location)

    def add_rules(self, **kwargs):
        """
        Add rules to the grammar.  The keyword arguments will provide a name to
        rules.

        :param dict[str, Parser] kwargs: The rules to add to the grammar.
        """
        import ast
        loc = extract_library_location()

        class GetTheCall(ast.NodeVisitor):
            """
            Helper visitor that will get the corresponding add_rule call in the
            source, so that we're then able to extract the precise line where
            each rule is added.
            """
            def __init__(self):
                self.the_call = None

            def visit_Call(self, call):
                if (
                    isinstance(call.func, ast.Attribute)
                    and call.func.attr == 'add_rules'
                    # Traceback locations are very imprecise, and python's ast
                    # doesn't have an end location for nodes, so we'll keep
                    # taking add_rules call, and the last is necessarily the
                    # good one.
                    and call.lineno <= loc.line
                ):
                    self.the_call = call

        the_call = GetTheCall()
        with open(loc.file) as f:
            file_ast = ast.parse(f.read(), f.name)
            the_call.visit(file_ast)

        # We're gonna use the keyword arguments to find back the precise line
        # where the rule was declared.
        keywords = {kw.arg: kw.value for kw in the_call.the_call.keywords}

        for name, rule in kwargs.items():
            rule.set_name(names.Name.from_lower(name))
            rule.set_grammar(self)
            rule.set_location(Location(loc.file, keywords[name].lineno, ""))
            rule.is_root = True

            with Context("In definition of rule '{}'".format(name), loc):
                check_source_language(
                    name not in self.rules,
                    "Rule '{}' is already present in the grammar".format(name)
                )

            self.rules[name] = rule

    def get_rule(self, rule_name):
        """
        Helper to return the rule corresponding to rule_name. The benefit of
        using this helper is that it will raise a helpful error diagnostic.

        :param str rule_name: The rule to get.
        """
        if rule_name not in self.rules:
            close_matches = difflib.get_close_matches(
                rule_name, self.rules.keys()
            )
            check_source_language(
                False, "Wrong rule name: '{}'. {}".format(
                    rule_name,
                    "Did you mean '{}'?".format(close_matches[0])
                    if close_matches else ""
                )
            )
        return self.rules[rule_name]

    def __getattr__(self, rule_name):
        """
        Build and return a Defer parser that references the above rule.

        :param str rule_name: The name of the rule.
        """
        return Defer(rule_name, lambda: self.get_rule(rule_name))

    def get_unreferenced_rules(self):
        """
        Return a set of names for all rules that are not transitively
        referenced by the main rule.

        :rtype: set[str]
        """
        # We'll first build the set of rules that are referenced, then we'll
        # know the ones not referenced.
        referenced_rules = set()

        def visit_parser(parser):
            """
            Visit all subparsers in "parser" and call "visit_rule" for Defer
            parsers.

            :param Parser parser: Parser to visit.
            """
            if isinstance(parser, Defer):
                visit_rule(parser.name)

            for sub_parser in parser.children():
                visit_parser(sub_parser)

        def visit_rule(rule_name):
            """
            Register "rule_name" as referenced and call "visit_parser" on the
            root parser that implements it. Do nothing if "rule_name" is
            already registered to avoid infinite recursion.

            :param str rule_name: Name for the rule to visit.
            """
            if rule_name in referenced_rules:
                return
            referenced_rules.add(rule_name)
            rule_parser = self.get_rule(rule_name)
            with rule_parser.diagnostic_context():
                visit_parser(rule_parser)

        # The following will fill "referenced_rules" thanks to recursion
        visit_rule(self.main_rule_name)

        return set(self.rules) - referenced_rules

    def warn_unreferenced_parsing_rules(self, context):
        """
        Emit a warning for unreferenced parsing rules.

        :type context: langkit.compile_context.CompileCtx
        """
        unreferenced_rules = self.get_unreferenced_rules()

        check_source_language(
            not unreferenced_rules, "The following parsing rules are not "
            "used: {}".format(", ".join(sorted(unreferenced_rules))),
            severity=Severity.warning
        )

    def check_main_rule(self, context):
        """
        Emit an error if the main parsing rule is missing.

        :type context: langkit.compile_context.CompileCtx
        """
        if not self.rules.get(self.main_rule_name, None):
            close_matches = difflib.get_close_matches(
                self.main_rule_name, self.rules.keys()
            )

            with self.context():
                check_source_language(
                    False,
                    'Invalid rule name specified for main rule: "{}". '
                    '{}'.format(
                        self.main_rule_name,
                        'Did you mean "{}"?'.format(close_matches[0])
                        if close_matches else ""
                    )
                )


class Parser(object):
    """
    Base class for parsers building blocks.
    """

    def __init__(self):
        self.location = None
        self._mod = None
        self.gen_fn_name = gen_name(self.base_name)
        self.grammar = None
        self.is_root = False
        self._name = names.Name("")

    def traverse_create_vars(self, start_pos):
        """
        This method will traverse the parser tree and create variables for
        every parser. When this has finished running, every parser should have
        a pos_var and a res_var.

        It leverages two callbacks that can be implemented by Parser
        subclasses: create_vars_before and create_vars_after.
        """
        self.start_pos = start_pos
        children_start_pos = self.create_vars_before() or start_pos
        for c in self.children():
            c.traverse_create_vars(children_start_pos)
        self.create_vars_after(children_start_pos)

    def create_vars_after(self, start_pos):
        """
        This callback can be implemented by parser subclasses, to create
        variables needed for parsing. If the parser needs to use the start_pos
        variable for either pos_var or res_var, this callback is the way to do
        it.
        """
        pass

    def create_vars_before(self):
        """
        This callback can be implemented by parser subclasses, to create
        variables needed for parsing. If the parser needs to override the
        position variable for its sub-parsers, it should override this callback
        and return the new position variable.

        :rtype: None|VarDef
        """
        pass

    def init_vars(self, pos_var=None, res_var=None):
        self.pos_var = (pos_var or VarDef(
            "{}_pos".format(self.__class__.__name__.lower()), Token
        ))

        self.res_var = (res_var or VarDef(
            "{}_res".format(self.__class__.__name__.lower()), self.get_type()
        ))

    @property
    def error_repr(self):
        """
        Return a representation of the parser suitable for generated code's
        error messages. Default implementation is to just use __repr__.
        """
        return repr(self)

    @property
    def base_name(self):
        """
        Return a simple name (names.Name instance) for this parser.

        The result is used as a base name for the generated function name.
        """
        return names.Name.from_camel(type(self).__name__ + "Parse")

    @property
    def name(self):
        return self._name.lower

    def discard(self):
        return False

    def __or__(self, other):
        """Return a new parser that matches this one or `other`."""

        # Optimization: if we are building an `Or` parser out of other `Or`
        # parsers, flatten the result.

        # Here, we used to mutate existing parsers instead of cloning them.
        # This is bad since parsers can be shared, and user expect such
        # combinatory operations to create new parsers without affecting
        # existing ones.

        alternatives = []
        other_parser = resolve(other)

        if isinstance(self, Or):
            alternatives.extend(self.parsers)
        else:
            alternatives.append(self)

        if isinstance(other_parser, Or):
            alternatives.extend(other_parser.parsers)
        else:
            alternatives.append(other_parser)

        return Or(*alternatives)

    def __xor__(self, transform_fn):
        """
        :type transform_fn: (T) => U
        :rtype: Transform
        """
        return Transform(self, transform_fn)

    def set_location(self, location):
        """
        Set the source location where this parser is defined. This is useful
        for error reporting purposes.

        :type location: langkit.diagnostics.Location
        """
        self.location = location
        for c in self.children():
            c.set_location(self.location)

    def diagnostic_context(self):
        """
        Helper that will return a diagnostic context manager with parameters
        set for the grammar definition.
        """
        return Context("In definition of grammar rule {}".format(self.name),
                       self.location)

    def set_grammar(self, grammar):
        """
        Associate `grammar` to this parser and to all its children.

        :param Grammar grammar: The grammar instance.
        """
        for c in self.children():
            c.set_grammar(grammar)
        self.grammar = grammar

    def set_name(self, name):
        """
        Rename this parser and all its children so that `name` is part of the
        corresponding function in the generated code.

        :param names.Name name: The name to include in the name of this parser
            tree.
        """
        for c in self.children():
            if not c._name and not isinstance(c, Defer):
                c.set_name(name)

        self._name = name
        self.gen_fn_name = gen_name(name + self.base_name)

    def is_left_recursive(self):
        """Return whether this parser is left-recursive."""
        return self._is_left_recursive(self.name)

    def _is_left_recursive(self, rule_name):
        """
        Private function used only by is_left_recursive, will explore the
        parser tree to verify whether the named parser with name rule_name is
        left recursive or not.
        """
        raise NotImplementedError()

    def children(self):
        """
        Parsers are combined to create new and more complex parsers.  They make
        up a parser tree.  Return a list of children for this parser.

        Subclasses must override this method.
        """
        raise NotImplementedError()

    def compute_fields_types(self):
        """
        Infer ASTNodeType's fields from this parsers tree.

        This method recurses over child parsers.  Parser subclasses must
        override this method if they contribute to fields typing.
        """
        for child in self.children():
            child.compute_fields_types()

    def compile(self):
        """
        Emit code for this parser as a function into the global context.

        :param langkit.compile_context.CompileCtx context: Global context.
        """
        context = get_context()

        check_source_language(
            self.get_type() is not None
            and self.get_type().is_ast_node,
            'Grammar rules must yield an AST node'
        )

        # Don't emit code twice for the same parser
        if self in context.fns:
            return
        context.fns.add(self)

        with add_var_context() as var_context:
            pos_var = VarDef("pos", Token, create=False)
            self.traverse_create_vars(pos_var)
            t_env = TemplateEnvironment(
                parser=self,
                code=self.generate_code(),
                var_context=var_context

            )

            context.generated_parsers.append(GeneratedParser(
                self.gen_fn_name,
                render('parsers/fn_profile_ada', t_env),
                render('parsers/fn_code_ada', t_env)
            ))

    def get_type(self):
        """
        Return a descriptor for the type this parser returns in the generated
        code.  It can be either the Token class or a CompiledType subtype.

        Subclasses must override this method.
        """
        raise NotImplementedError()

    def generate_code(self):
        """
        Return generated code for this parser into the global context.

        Subclasses must override this method.
        """
        raise NotImplementedError()

    def render(self, template_name, **kwargs):
        """
        Shortcut for render for parsers, passing the parsers sub-path for
        templates, and self as "parser" in the template context.
        """
        return render(
            "parsers/{}".format(template_name), parser=self, **kwargs
        )

    @property
    def symbol_literals(self):
        """
        Return a list of strings for all symbol literals used by this parser
        and all its subparsers.

        Subclasses must override this method to actually include their own
        symbol literals.

        :rtype: set[str]
        """
        result = set()
        for child in self.children():
            result.update(child.symbol_literals)
        return result

    def add_symbol_literals(self):
        """
        Register symbols literals used by this parser to "context".

        :type context: langkit.compile_context.CompileCtx
        """
        context = get_context()
        for sym in self.symbol_literals:
            context.add_symbol_literal(sym)


class Tok(Parser):
    """
    Parser that matches a specific token.
    """

    def children(self):
        return []

    @property
    def error_repr(self):
        return '{}'.format(
            self.val.matcher.to_match if self.val.matcher else self.val
        )

    def __repr__(self):
        return "Tok({0})".format(repr(self.val))

    def discard(self):
        return not self.keep

    def _is_left_recursive(self, rule_name):
        return False

    def __init__(self, val, keep=False, match_text=""):
        """
        Create a parser that matches a specific token.

        :param TokenAction|str val: Either a reference to a TokenAction that is
            part of your lexer definition, either a string that will be used to
            find back the proper TokenAction.

        :param str match_text: If val is a WithSymbol token action, allows to
            specify the exact text that should be matched by this parser.
        """
        Parser.__init__(self)

        self._val = val
        ":type: TokenAction|str"

        self.match_text = match_text
        ":type: str"

        check_source_language(
            not self.match_text or isinstance(self._val, WithSymbol),
            "Tok matcher has match text, but val is not a WithSymbol instance,"
            " got {} instead".format(val)
        )

        self.keep = keep

    def get_type(self):
        return Token

    def create_vars_after(self, start_pos):
        self.init_vars()

    @property
    def val(self):
        """
        Return the token action associated with this Tok parser.

        :rtype: TokenAction
        """
        return (get_context().lexer.get_token(self._val)
                if isinstance(self._val, basestring)
                else self._val)

    def generate_code(self):
        return self.render('tok_code_ada', token_kind=self.val.ada_name)

    @property
    def matches_symbol(self):
        """
        Return whether this parser matches over a symbol's text.

        :rtype: bool
        """
        return self.match_text and isinstance(self.val, WithSymbol)

    @property
    def symbol_literals(self):
        return {self.match_text} if self.matches_symbol else set()


class Or(Parser):
    """Parser that matches what the first sub-parser accepts."""

    def _is_left_recursive(self, rule_name):
        return any(parser._is_left_recursive(rule_name)
                   for parser in self.parsers)

    def __repr__(self):
        return "Or({0})".format(", ".join(repr(m) for m in self.parsers))

    def __init__(self, *parsers):
        """
        Create a parser that matches any thing that the first parser in
        `parsers` accepts.

        :type parsers: list[Parser|Token|type]
        """
        Parser.__init__(self)
        self.parsers = [resolve(m) for m in parsers]

        # Typing resolution for this parser is a recursive process.  So first
        # we need to prevent infinite recursions (because of recursive
        # grammars)...
        self.is_processing_type = False

        # ... and we want to memoize the result.
        self.cached_type = None

    def children(self):
        return self.parsers

    def get_type(self):
        if self.cached_type:
            return self.cached_type

        # Callers are already visiting this node, so we cannot return its type
        # right now.  Return None so that it doesn't contribute to type
        # resolution.
        if self.is_processing_type:
            return None

        try:
            self.is_processing_type = True
            types = set()
            for m in self.parsers:
                t = m.get_type()
                if t:
                    types.add(t)

            # There are two possibilities:
            #  - if all alternatives return AST nodes: then this parser's
            #    return type is the common ancestor for all of these.
            #  - otherwise, make sure that all alternatives return exactly the
            #    same type.
            if all(t.is_ast_node for t in types):
                res = common_ancestor(*types)
            else:
                typs = list(types)
                ref_type = typs[0]

                check_source_language(
                    all(t == ref_type for t in typs),
                    'Alternatives yield incompatible types: {}'.format(
                        ', '.join(sorted(t.name().camel for t in typs))
                    )
                )
                res = ref_type

            self.cached_type = res
            return res
        finally:
            self.is_processing_type = False

    def create_vars_after(self, start_pos):
        self.init_vars()

    def generate_code(self):
        return self.render('or_code_ada', exit_label=gen_name("Exit_Or"))


def always_make_progress(parser):
    """
    Return whether `parser` cannot match an empty sequence of tokens.

    :param Parser parser: The parser to evaluate.
    """
    if isinstance(parser, List):
        return not parser.empty_valid or always_make_progress(parser.parser)
    return not isinstance(parser, (Opt, Null))


def Pick(*parsers):
    """
    Utility around Row and Extract, that will automatically scan a Row, remove
    tokens and ignored sub parses, and extract the only significant sub-result.

    If there are several significant sub-results, raises an error.
    """
    location = extract_library_location()
    parsers = [resolve(p) for p in parsers if p]
    pick_parser_idx = -1
    for i, p in enumerate(parsers):
        if p.discard():
            continue
        with Context("", location):
            check_source_language(
                pick_parser_idx == -1,
                "Pick parser can have only one sub-parser that is not a token",
                Severity.non_blocking_error
            )
        pick_parser_idx = i

    if pick_parser_idx == -1:
        return Row(*parsers)
    else:
        return Row(*parsers)[pick_parser_idx]


class Row(Parser):
    """
    Parser that matches a what sub-parsers match in sequence.
    """

    @property
    def error_repr(self):
        return " ".join(m.error_repr for m in self.parsers)

    def _is_left_recursive(self, rule_name):
        for parser in self.parsers:
            res = parser._is_left_recursive(rule_name)
            if res:
                return True
            if always_make_progress(parser):
                break
        return False

    def __repr__(self):
        return "Row({0})".format(", ".join(repr(m) for m in self.parsers))

    def __init__(self, *parsers):
        """
        Create a parser that matches the sequence of matches for all
        sub-parsers in `parsers`.

        If a parser is none it will be ignored. This allows to create
        programmatic helpers that generate rows more easily.

        :type parsers: list[Parser|types.Token|type]
        """
        Parser.__init__(self)

        self.parsers = [resolve(m) for m in parsers if m]

        # The type this row returns is initialized either when assigning a
        # wrapper parser or when trying to get the type (though the get_type
        # method) while no wrapper has been assigned.
        self.typ = None

    def discard(self):
        return all(p.discard() for p in self.parsers)

    def children(self):
        return self.parsers

    def get_type(self):
        # A Row parser never yields a concrete result itself
        return None

    def create_vars_before(self):
        # We pass in a dummy object for res_var, because Rows have no result
        self.init_vars(res_var=object())
        return self.pos_var

    def create_vars_after(self, pos_var):
        self.subresults = [p.res_var if not p.discard() else None
                           for p in self.parsers]

    def generate_code(self):
        return self.render('row_code_ada', exit_label=gen_name("Exit_Row"))

    def __getitem__(self, index):
        """
        Return a parser that matches `self` and that discards everything except
        the `index`th field in the row.
        """
        return Extract(self, index)


class List(Parser):
    """
    Parser that matches a list.  A sub-parser matches list items.
    """

    def _is_left_recursive(self, rule_name):
        res = self.parser._is_left_recursive(rule_name)
        assert not(
            res and (self.empty_valid or not always_make_progress(self.parser))
        )
        return res

    def __repr__(self):
        return "List({0})".format(
            repr(self.parser) + (", sep={0}".format(self.sep)
                                 if self.sep else "")
        )

    def __init__(self, *parsers, **opts):
        """
        Create a parser that matches a list of elements.

        `parsers` is one or several sub-parsers. If several are passed, then
        they're automatically wrapped in a `Pick` parser, so that only one
        result is kept.

        Each element will be matched by `parsers`.  If `sep` is provided, it is
        a parser that is used to match separators between elements.

        By default, this parser will not match empty sequences but it will if
        `empty_valid` is True.

        :param ASTNodeType list_cls: Type parameter. If provided, it must be a
            ASTNodeType.list_type() subclass to be used for the result of this
            parser.

        :param types.Token|string sep: Parser or string corresponding to the
            token that is used to match separators between elements.

        :param bool empty_valid: Whether to match empty sequences or not.
        """

        Parser.__init__(self)
        if len(parsers) == 1:
            # If one parser, just keep it as the main parser
            self.parser = resolve(parsers[0])
        else:
            # If several, then wrap them in a Pick parser
            self.parser = Pick(*parsers)

        sep = opts.get('sep')
        self.sep = resolve(sep) if sep else None
        self.empty_valid = opts.get('empty_valid', False)
        self.list_cls = opts.get('list_cls', None)

    def children(self):
        return keep([self.parser, self.sep])

    def get_type(self):
        with self.diagnostic_context():
            if self.list_cls:
                ret = self.list_cls
                check_source_language(
                    ret.is_list_type,
                    'Invalid list type for List parser: {}. '
                    'Not a list type'.format(ret.name().camel)
                )
                return ret
            else:
                item_type = self.parser.get_type()
                check_source_language(
                    item_type.is_ast_node,
                    'List parsers only accept subparsers that yield AST nodes'
                    ' ({} provided here)'.format(item_type.name().camel)
                )
                return item_type.list_type()

    def compute_fields_types(self):
        Parser.compute_fields_types(self)

        typ = self.get_type()
        with self.diagnostic_context():
            check_source_language(
                not typ.abstract,
                'Please provide a concrete ASTnode subclass as list_cls'
                ' ({} is abstract)'.format(typ.name().camel)
            )

    def create_vars_before(self):
        self.cpos = VarDef("lst_cpos", Token)
        self.tmplist = VarDef('tmp_list', 'Free_Parse_List')
        return self.cpos

    def create_vars_after(self, start_pos):
        self.init_vars()

    def generate_code(self):
        return self.render('list_code_ada')


class Opt(Parser):
    """
    Parser that matches something if possible or that matches an empty sequence
    otherwise.
    """

    @property
    def error_repr(self):
        return "[{}]".format(self.parser.error_repr)

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self):
        return "Opt({0})".format(self.parser)

    def __init__(self, parser, *parsers):
        """
        Create a parser that matches `parser` and then `parsers` if possible or
        matches an empty sequence otherwise.  The result is equivalent to::

            Opt(Row(parser, *parsers)).
        """
        Parser.__init__(self)

        self._booleanize = None
        """
        Three-tuple of types, used to determine how the result will be
        booleanized:

        * If the first type is an abstract node class, and the two others are
          derived classes with no fields, then the second will be generated for
          the True node, and the third for the False node.

        * If the first type is BoolType, then the result is booleanized into a
          regular boolean.
        :type: (CompiledType, CompiledType, CompiledType)
        """

        self._is_error = False
        self.contains_anonymous_row = bool(parsers)
        self.parser = Pick(parser, *parsers) if parsers else resolve(parser)

    def discard(self):
        return (not self._booleanize) and self.parser.discard()

    def error(self):
        """
        Returns the self parser, modified to function as an error recovery
        parser.

        The semantic of Opt in this case is that it will try to parse it's
        sub parser, and when failing, it will add a diagnostic to the
        parser's diagnostic list.

        NOTE: There is no diagnostics backtracking if the parent parser is
        discarded. That means that you should only use this parser in cases
        in which you are sure that you are in a successfull branch of your
        parser. This is neither checked statically nor dynamically so use
        with care!

        :rtype: Opt
        """
        return copy_with(self, _is_error=True)

    def as_bool(self, dest=None):
        """
        Returns the self parser, modified to return a bool rather than the
        sub-parser result. The result will be true if the parse was
        successful, false otherwise.

        This is typically useful to store specific tokens as attributes,
        for example in Ada, you'll mark a subprogram as overriding with the
        "overriding" keyword, and we want to store that in the tree as a
        boolean attribute, so we'll use::

            Opt("overriding").as_bool()

        :param CompiledType|None dest: If not None, then it is expected that
            this is an EnumType with qualifier = True. In this cases, the
            result will be booleanized.

        :rtype: Opt
        """
        if dest is None:
            base, alt_true, alt_false = (BoolType, BoolType, BoolType)
        else:
            base = assert_type(dest, ASTNodeType)
            assert base.is_bool_node
            alt_true, alt_false = base._alternatives

        new = copy_with(self, _booleanize=(base, alt_true, alt_false))
        return new

    def children(self):
        return [self.parser]

    def get_type(self):
        return (
            self._booleanize[0] if self._booleanize else self.parser.get_type()
        )

    def create_vars_after(self, start_pos):
        self.init_vars(
            self.parser.pos_var,
            res_var=None if self._booleanize else self.parser.res_var
        )

    def generate_code(self):
        return self.render('opt_code_ada')

    def __getitem__(self, index):
        """Same as Row.__getitem__:
        Return a parser that matches `self` and that discards everything except
        the `index`th field in the row.

        Used as a shortcut, will only work if the Opt's sub-parser is a row.
        """
        m = self.parser
        assert isinstance(m, Row)
        return Opt(Extract(m, index))


class Extract(Parser):
    """
    Wrapper parser used to discard everything from a Row parser except a single
    field in it.
    """

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    @property
    def error_repr(self):
        return self.parser.error_repr

    def __repr__(self):
        return "Extract({0}, {1})".format(self.parser, self.index)

    def __init__(self, parser, index):
        """
        :param Row parser: The parser that will serve as target for
            extract operation.
        :param int index: The index you want to extract from the row.
        """
        Parser.__init__(self)
        self.parser = parser
        self.index = index
        assert isinstance(self.parser, Row)

    def children(self):
        return [self.parser]

    def get_type(self):
        return self.parser.parsers[self.index].get_type()

    def create_vars_after(self, start_pos):
        self.init_vars(
            self.parser.pos_var, self.parser.subresults[self.index]
        )

    def generate_code(self):
        return self.parser.generate_code()


class Discard(Parser):
    """
    Wrapper parser used to discard the match.
    """

    def discard(self):
        return True

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self):
        return "Discard({0})".format(self.parser)

    def __init__(self, parser):
        Parser.__init__(self)

        parser = resolve(parser)
        self.parser = parser

    def children(self):
        return [self.parser]

    def get_type(self):
        # Discard parsers return nothing!
        return None

    def create_vars_after(self, start_pos):
        self.init_vars(self.parser.pos_var, self.parser.res_var)

    def generate_code(self):
        return self.parser.generate_code()


class Defer(Parser):
    """
    Stub parser used to implement forward references.
    """

    def children(self):
        # We don't resolve the Defer's pointed parser here, because that would
        # transform the parser tree into a graph.
        return []

    @property
    def error_repr(self):
        return self.name

    @property
    def parser(self):
        if not self._parser:
            self._parser = self.parser_fn()
        return self._parser

    @property
    def name(self):
        # Don't rely on `self.parser` since it may not be available right now
        # (that's why it is deferred in the first place).
        return self.rule_name

    def _is_left_recursive(self, rule_name):
        return self.name == rule_name

    def __repr__(self):
        return "{0}".format(self.name)

    def __init__(self, rule_name, parser_fn):
        """
        Create a stub parser.

        :param str rule_name: the name of the deferred parser (used for
            pretty-printing).
        :param callable parser_fn: must be a callable that returns the
            referenced parser.
        """
        Parser.__init__(self)
        self.rule_name = rule_name
        self.parser_fn = parser_fn
        self._parser = None
        ":type: Parser"

    def get_type(self):
        return self.parser.get_type()

    def create_vars_after(self, start_pos):
        self.init_vars()

    def generate_code(self):
        # The call to compile will add the declaration and the definition
        # (body) of the function to the compile context.
        self.parser.compile()

        # Generate a call to the previously compiled function, and return
        # the context corresponding to this call.
        return self.render('fn_call_ada')


class Transform(Parser):
    """
    Wrapper parser for a Row parser used to instantiate an AST node.
    """

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self):
        return "{0} ^ {1}".format(self.parser, self.typ.name().camel)

    def __init__(self, parser, typ):
        """
        Create a Transform parser wrapping `parser` and that instantiates AST
        nodes whose type is `typ`.
        """
        Parser.__init__(self)
        assert isinstance(typ, ASTNodeType) or typ.is_ast_node

        self.parser = parser
        self.typ = typ
        self._is_ptr = typ.is_ptr

    def children(self):
        return [self.parser]

    def get_type(self):
        return self.typ

    def compute_fields_types(self):
        # Gather field types that come from all child parsers
        if isinstance(self.parser, Row):
            # There are multiple fields for Row parsers
            fields_types = [parser.get_type()
                            for parser in self.parser.parsers
                            if not parser.discard()]
        elif isinstance(self.parser, Tok) and not self.parser.keep:
            fields_types = []
        else:
            fields_types = [self.parser.get_type()]

        assert all(t for t in fields_types), (
            "Internal error when computing field types for {}:"
            " some are None: {}".format(self.typ, fields_types)
        )

        # Check that the number of values produced by self and the number of
        # fields in the destination node are the same.
        nb_transform_values = len(fields_types)
        nb_fields = len(self.typ.get_parse_fields())
        check_source_language(
            nb_transform_values == nb_fields,
            'Transform parser generates {} values, but {} has {} fields'
            .format(nb_transform_values, self.typ.name().camel, nb_fields)
        )

        # Propagate types from self to destination node's fields
        self.typ.set_types(fields_types)

        # Handle sub-parsers
        Parser.compute_fields_types(self)

    def create_vars_after(self, start_pos):
        self.init_vars(self.parser.pos_var)

    def generate_code(self):
        return self.render(
            'transform_code_ada',
            args=(keep(self.parser.subresults)
                  if isinstance(self.parser, Row) else [self.parser.res_var]),
        )


class Null(Parser):
    """
    Parser that matches the empty sequence and that yields no AST node.
    """

    def __init__(self, result_type):
        """
        Create a new Null parser.  `result_type` is either a CompiledType
        subclass that defines what nullexpr this parser returns, either a
        Parser subclass' instance.  In the latter case, this parser will return
        the same type as the other parser.
        """
        Parser.__init__(self)
        if isinstance(result_type, (CompiledType, Parser)):
            self.typ = result_type
        elif issubclass(result_type, CompiledType):
            self.typ = result_type
        else:
            raise TypeError(
                'Invalid result type for Null parser: {}'.format(result_type))

    def children(self):
        return []

    def _is_left_recursive(self, rule_name):
        return False

    def __repr__(self):
        return "Null"

    def create_vars_after(self, start_pos):
        self.init_vars(start_pos)

    def generate_code(self):
        return self.render('null_code_ada')

    def get_type(self):
        return (
            self.typ.get_type()
            if isinstance(self.typ, Parser) else
            self.typ
        )


class Enum(Parser):
    """
    Wrapper parser used to return an enumeration value for a match.
    """

    def _is_left_recursive(self, rule_name):
        if self.parser:
            return self.parser._is_left_recursive(rule_name)
        return False

    def __repr__(self):
        return "Enum({0}, {1})".format(self.parser, self.enum_type_inst)

    def __init__(self, parser, enum_type_inst):
        """
        Create a wrapper parser around `parser` that returns `enum_type_inst`
        (an EnumType subclass instance) when matching.
        """
        Parser.__init__(self)
        self.parser = resolve(parser) if parser else None
        ":type: Parser|Row"

        self.enum_type_inst = enum_type_inst

    def create_vars_after(self, start_pos):
        self.init_vars(
            pos_var=self.parser.pos_var if self.parser else start_pos
        )

    def children(self):
        return keep([self.parser])

    def get_type(self):
        return type(self.enum_type_inst)

    def generate_code(self):
        return self.render('enum_code_ada')


_ = Discard

# This part of the file contains experiments toward automatic generation of
# pretty printers for Langkit grammars.


class NodeToParsersPass():
    """
    This pass computes the correspondence between node-types and parsers. The
    end goal is to have one and only one non-ambiguous rule to pretty-print an
    AST type.
    """

    def __init__(self):
        self.nodes_to_rules = defaultdict(list)
        self.can_create_pp = True
        self.canonical_rules = {}

    def check_nodes_to_rules(self, ctx):
        """
        Check the results of the compute pass, to see if every node type only
        has one non ambiguous way of being pretty printed, and assign a
        canonical representation to every node type.
        """
        from langkit.compiled_types import StructMetaclass

        for node_type in StructMetaclass.astnode_types:
            with node_type.diagnostic_context():
                WarningSet.unused_node_type.warn_if(
                    node_type not in self.nodes_to_rules.keys()
                    and not node_type.abstract
                    and not node_type.synthetic,
                    "{} has no parser, and is marked neither abstract nor "
                    "synthetic".format(node_type.name())
                )

        for node, parsers in self.nodes_to_rules.items():
            if len(parsers) > 1:
                if not pp_struct_eq(parsers):
                    WarningSet.pp_bad_grammar.warn_if(
                        True,
                        "Node {} is parsed in different incompatible "
                        "ways. This will prevent the generation of an "
                        "automatic pretty printer.\nFor more information, "
                        "enable the pp_eq trace".format(node.name()),
                    )
                    Log.log("pp_eq", "for node {} is false".format(node))
                    with Log.nest():
                        for val in parsers:
                            Log.log("pp_eq", val)
                    Log.log("pp_eq")
                    self.can_create_pp = False

                self.canonical_rules[node] = find_canonical_parser(parsers)
            else:
                self.canonical_rules[node] = parsers[0]

            Log.log("pp_canonical", node.name(),
                    self.canonical_rules[node])

            # Set the canonical parser on this ASTNodeType type
            node.parser = self.canonical_rules[node]

    def compute(self, parser):
        """
        This is a grammar-rules pass implementation, whose goal is to map each
        node type to one specific parser.
        """

        def compute_internal(parser):
            if creates_node(parser, follow_refs=False):
                if isinstance(parser, Opt) and parser._booleanize:
                    for alt in parser.get_type()._alternatives:
                        self.nodes_to_rules[alt].append(parser)

                self.nodes_to_rules[parser.get_type()].append(parser)

            for c in parser.children():
                compute_internal(c)

        WarningSet.pp_bad_grammar.warn_if(
            not creates_node(parser),
            "'{}' toplevel rule loses information. Prevents "
            "generation of the pretty-printer".format(parser.name)
        )
        compute_internal(parser)


def creates_node(p, follow_refs=True):
    """
    Predicate that is true on parsers that create a node directly, or are just
    a reference to one or several parsers that creates nodes, without
    additional parsing involved.

    For example::
        Node(..)               # <- True
        Or(a, b, c)            # <- True if a b & c creates_node
        Row(a, b, c)           # <- False
        Pick(";", "lol", c)    # <- False
    """
    if isinstance(p, Or) and follow_refs:
        return all(creates_node(c) for c in p.children())

    if isinstance(p, Defer) and follow_refs:
        return p.get_type().matches(ASTNodeType)

    if isinstance(p, Opt) and follow_refs and creates_node(p.parser):
        return True

    return (
        isinstance(p, Transform)
        or isinstance(p, List)
        or isinstance(p, Opt) and (
            p._booleanize and p._booleanize[0].matches(ASTNodeType)
        )
    )


@Log.recursive
@Log.log_return('pp_eq_impl')
def pp_struct_eq(parsers, toplevel=True):
    """
    Determine if two parsers are structurally equal, with regards to pretty
    printing.

    :type parser: Parser
    :type other_parser: Parser
    """

    Log.log("pp_eq_impl", "parsers: ".format(parsers))

    # If there is only one parser, the result is obviously True
    if len(parsers) == 1:
        return True

    parsers_types = set(type(p) for p in parsers)

    # First, if there are Null parsers in the lot, we can safely ignore them,
    # and run the algorithm on the remaining parsers.
    if Null in parsers_types:
        if is_same(p.get_type() for p in parsers):
            return pp_struct_eq(
                [p for p in parsers if not isinstance(p, Null)]
            )

    # All parsers are of the same kind. Let's see if they're structurally
    # equivalent.
    if len(parsers_types) == 1:
        # We pick the type out of the set
        typ = parsers_types.pop()

        # For those parser kinds, we want to check if their children are the
        # same.
        if typ in (Row, Transform, List, Opt):
            children_lists = [p.children() for p in parsers]
            return is_same(len(c) for c in children_lists) and all(
                pp_struct_eq(c, False)
                for c in zip(*children_lists)
            )
        # For tok, we want to check that the parsed token is the same
        elif typ == Tok:
            return is_same(p.val for p in parsers)
        # For extract, structural equality involves comparing indices too
        elif typ == Extract:
            return pp_struct_eq(
                [p.parser for p in parsers]
            ) and is_same(p.index for p in parsers)
        # Defer and Or will be handled by the same logic we use when the kind
        # of parser is not unique.
        elif typ in (Defer, Or):
            pass
        else:
            raise Exception("Parser type not handled")

    # If we fall down here, either:
    # 1. There are more than one parser kind.
    # 2. The kind is one of those not handled by the block of code above (Or
    #    and Defer).

    # We will use a specific logic for sub-parsers (toplevel=False): If they
    # all create nodes directly, without adding additional parser logic, then
    # their uniqueness is already checked because we call
    # pp_struct_eq on all of those.
    if not toplevel:
        resolved_parsers = [
            p.parser if isinstance(p, Defer) else p for p in parsers
        ]
        return all(creates_node(p) for p in resolved_parsers)

    return False


def find_canonical_parser(parsers):
    """
    From a list of parsers corresponding to the same node type, return the one
    that will be used to emit the pretty-printer, which is considered the
    canonical one for pretty-printing.

    :rtype: Parser
    """
    def has_null(parser):
        """
        Return true if parser contains a null parser somewhere.
        """
        return isinstance(parser, Null) or any(
            has_null(c) for c in parser.children()
        )

    nulls, no_nulls = split(has_null, parsers)
    return no_nulls[0] if no_nulls else nulls[0]
