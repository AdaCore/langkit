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

from contextlib import contextmanager
import difflib
from funcy import keep
import inspect

from langkit import compiled_types, names
from langkit.common import gen_name
from langkit.compile_context import get_context
from langkit.compiled_types import ASTNodeType, T, resolve_type
from langkit.diagnostics import (
    Context, Location, check_source_language, extract_library_location,
    Severity
)
from langkit.expressions import resolve_property
from langkit.lexer import TokenAction, WithSymbol
from langkit.utils import (assert_type, copy_with, issubtype,
                           type_check_instance)


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
    """

    def __init__(self, base_name, type, create=True, reinit=False):
        """
        Creates a VarDef.

        :param str base_name: The string used as base for the name of the
            variable. A number will be appended in order to make the variable
            unique.

        :param CompiledType type: The type of the variable.

        :param bool create: If true, the variable will be automatically added
            to the current variable context when created, and an unique name
            will be generated. If false, it is considered that the definition
            is done in the template, so the name used is the name passed, and
            the variable won't be added to the context.

        :param bool reinit: If true, the variable will be reinitialized to null
            expr if variable occurs in a left recursive parser, and left
            recursion is triggered.
        """

        self.type = type
        self.reinit = reinit

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

    def __str__(self):
        """
        Helper, expand var defs to their name when converted to strings,
        allows easy use of VarDef instances in code generation.
        """
        return str(self.name)

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
    from langkit.unparsers import (
        ListNodeUnparser, RegularNodeUnparser, TokenNodeUnparser
    )

    return compiled_types.make_renderer().update({
        'is_tok':       type_check_instance(_Token),
        'is_row':       type_check_instance(_Row),
        'is_dontskip':  type_check_instance(DontSkip),
        'is_defer':     type_check_instance(Defer),
        'is_transform': type_check_instance(_Transform),
        'is_list':      type_check_instance(List),
        'is_opt':       type_check_instance(Opt),
        'is_null':      type_check_instance(Null),
        'is_nobt':      type_check_instance(NoBacktrack),
        'is_extract':   type_check_instance(_Extract),
        'is_class':     inspect.isclass,
        'ctx':          get_context(),

        'is_regular_node_unparser': type_check_instance(RegularNodeUnparser),
        'is_list_node_unparser': type_check_instance(ListNodeUnparser),
        'is_token_node_unparser': type_check_instance(TokenNodeUnparser),
    }).render(*args, **kwargs)


def resolve(parser):
    """
    :type parser: Parser|ParserContainer
    :rtype: Parser
    """
    if isinstance(parser, Parser):
        return parser
    elif isinstance(parser, basestring):
        return _Token(parser)
    elif isinstance(parser, TokenAction):
        return _Token(parser)
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

        # Rules added internally (DontSkip rules for example) might not have a
        # location. So test loc first.
        if loc:
            the_call = GetTheCall()
            with open(loc.file) as f:
                file_ast = ast.parse(f.read(), f.name)
                the_call.visit(file_ast)

            # We're gonna use the keyword arguments to find back the precise
            # line where the rule was declared.
            keywords = {kw.arg: kw.value for kw in the_call.the_call.keywords}

        for name, rule in kwargs.items():
            rule = resolve(rule)
            rule.set_name(names.Name.from_lower(name))
            rule.set_grammar(self)

            if loc and name in keywords:
                rule.set_location(Location(loc.file, keywords[name].lineno))

            rule.is_root = True

            with Context(
                "In definition of rule '{}'".format(name), rule.location
            ):
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

            for sub_parser in parser.children:
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
            with rule_parser.diagnostic_context:
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
        # Get the location of the place where this parser is created. This will
        # likely be overriden in Grammar.add_rules with a more precise
        # location if we can find the keyword argument in Python source code,
        # but if it is not, we have a degraded more.
        self.location = extract_library_location()

        self._mod = None
        self.gen_fn_name = gen_name(self.base_name)
        self.grammar = None
        self.is_root = False
        self._name = names.Name("")
        self.no_backtrack = None
        """
        :type: VarDef

        If this variable is set, it indicates that this parser is part of a
        no_backtrack hierarchy, indicating that if there is a failure, the
        parsers should not try to backtrack, but instead return a failure node.

        When compiling the parsers, we will check existence of this variable to
        know if there is a possibility of not backtracing. At runtime, parsers
        will check the value of this variable to know if they should backtrack
        or not.
        """

        self.is_dont_skip_parser = False
        """
        Whether this parser is a parser generated as part of a DontSkip parser,
        to scan the input to see if input should be skipped or not.
        """

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
        for c in self.children:
            c.traverse_create_vars(children_start_pos)
        self.create_vars_after(children_start_pos)

    def traverse_dontskip(self, grammar):
        """
        Traverse the parser, looking for DontSkip parsers. When finding one,
        create a dedicated separate rule for the parsers that should not be
        skipped, so that we can store references to those parsers as function
        pointers.
        """
        if isinstance(self, DontSkip):
            # The purpose of the parsers passed as argument to dont_skip is not
            # to actually generate a node, but to see if we can parse the
            # sequence or not. So we'll generate a fake stub node, and pick it.
            self.dontskip_parser = _pick_impl(
                [Null(get_context().root_grammar_class)]
                + list(self.dontskip_parsers),
                True
            )
            self.dontskip_parser.is_dont_skip_parser = True
            grammar.add_rules(**{
                gen_name('dontskip_{}'.format(self.name)).lower:
                self.dontskip_parser
            })

        for c in self.children:
            c.traverse_dontskip(grammar)

    def traverse_nobacktrack(self, nobt=None):
        """
        This method will traverse the parser hierarchy and set the no_backtrack
        variable if necessary, indicating which parsers should not backtrack.
        """
        if isinstance(self, NoBacktrack):
            if nobt:
                self.no_backtrack = nobt
            else:
                self.no_backtrack = VarDef('nobt', T.Bool, reinit=True)

        for c in self.children:
            nobt = c.traverse_nobacktrack(self.no_backtrack)
            # Or parsers are a stop point for nobacktrack

            if nobt and not isinstance(self, Or):
                self.no_backtrack = nobt

        return self.no_backtrack

    def create_vars_after(self, start_pos):
        """
        This callback can be implemented by parser subclasses, to create
        variables needed for parsing. If the parser needs to use the start_pos
        variable for either pos_var or res_var, this callback is the way to do
        it.
        """
        pass  # no-code-coverage

    def create_vars_before(self):
        """
        This callback can be implemented by parser subclasses, to create
        variables needed for parsing. If the parser needs to override the
        position variable for its sub-parsers, it should override this callback
        and return the new position variable.

        :rtype: None|VarDef
        """
        pass  # no-code-coverage

    @classmethod
    def parser_cls_name(cls):
        """
        Return a name for this parser class to be used for code generation.

        :rtype: str
        """
        return cls.__name__.strip('_')

    def init_vars(self, pos_var=None, res_var=None):
        base_name = self.parser_cls_name().lower()

        self.pos_var = pos_var or VarDef('{}_pos'.format(base_name), T.Token)
        self.res_var = res_var or VarDef('{}_res'.format(base_name),
                                         self.get_type())

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
        return names.Name.from_camel(self.parser_cls_name() + 'Parse')

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

    def set_location(self, location):
        """
        Set the source location where this parser is defined. This is useful
        for error reporting purposes.

        :type location: langkit.diagnostics.Location
        """
        self.location = location
        for c in self.children:
            c.set_location(self.location)

    @property
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
        for c in self.children:
            c.set_grammar(grammar)
        self.grammar = grammar

    def set_name(self, name):
        """
        Rename this parser and all its children so that `name` is part of the
        corresponding function in the generated code.

        :param names.Name name: The name to include in the name of this parser
            tree.
        """
        for c in self.children:
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

    @property
    def can_parse_token_node(self):
        """
        Return whether this parser can be used as a sub-parser to build token
        nodes.

        :rtype: bool
        """
        return False

    @property
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
        for child in self.children:
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
            pos_var = VarDef("pos", T.Token, create=False)

            # Compute no_backtrack information for this parser
            self.traverse_nobacktrack()
            self.traverse_create_vars(pos_var)
            t_env = {'parser': self,
                     'code': self.generate_code(),
                     'var_context': var_context}

            context.generated_parsers.append(GeneratedParser(
                self.gen_fn_name,
                render('parsers/fn_profile_ada', t_env),
                render('parsers/fn_code_ada', t_env)
            ))

    def get_type(self):
        """
        Return a descriptor for the type this parser returns in the generated
        code.

        Subclasses must override this method.

        :rtype: CompiledType
        """
        raise NotImplementedError()

    def generate_code(self):
        """
        Return generated code for this parser into the global context.

        Subclasses must override this method.

        :rtype: str
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
        for child in self.children:
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

    def dont_skip(self, *parsers):
        """
        Syntax sugar allowing to write::

            rule.dont_skip(Or("end", "begin"))

        Rather than::

            DontSkip(rule, Or("end", "begin"))
        """
        return DontSkip(self, *parsers)


class _Token(Parser):
    """
    Parser that matches a specific token.
    """

    @property
    def children(self):
        return []

    @property
    def error_repr(self):
        return '{}'.format(
            self.val.matcher.to_match if self.val.matcher else self.val
        )

    def __repr__(self):
        return "Token({0})".format(repr(self._val))

    def discard(self):
        return True

    def _is_left_recursive(self, rule_name):
        return False

    def __init__(self, val, match_text=""):
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

    @property
    def can_parse_token_node(self):
        return True

    def get_type(self):
        return T.Token

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


class Skip(Parser):
    """
    This recovery parser will skip any token and produce a node from it,
    generating an error along the way.

    Note that if you use that in any kind of List, this will wreck your parser
    if you don't use the associated DontSkip parser in a parent parser.
    """

    def __init__(self, dest_node):
        """
        :param CompiledType dest_node: The node type to create.
        """
        Parser.__init__(self)
        self.dest_node = dest_node
        self.dest_node_parser = dest_node()

    def __repr__(self):
        return 'Skip({})'.format(node_name(self.dest_node))

    @property
    def children(self):
        return [self.dest_node_parser]

    def generate_code(self):
        return self.render('skip_code_ada', exit_label=gen_name("Exit_Or"))

    def get_type(self):
        return resolve_type(self.dest_node)

    def create_vars_after(self, start_pos):
        self.init_vars(res_var=self.dest_node_parser.res_var)
        self.dummy_node = VarDef('skip_dummy', T.root_node)

    def _is_left_recursive(self, rule_name):
        return False


class DontSkip(Parser):
    """
    This is used in the following way::

        parser.dont_skip(other_parser)

    This means that in the scope of ``parser``, if a skip parser occurs, it
    will first run ``other_parser``, and if ``other_parser`` is successful,
    then skip will fail (eg. not skip anything).
    """

    def __init__(self, subparser, *dontskip_parsers):
        Parser.__init__(self)
        self.subparser = subparser
        self.dontskip_parsers = dontskip_parsers

    def __repr__(self):
        return 'DontSkip({}, {})'.format(self.subparser, self.dontskip_parsers)

    @property
    def children(self):
        return [self.subparser]

    def generate_code(self):
        return """
        Parser.Private_Part.Dont_Skip.Append
          ({dontskip_parser_fn}'Access);
        {subparser_code}
        Parser.Private_Part.Dont_Skip.Delete_Last;
        """.format(
            subparser_code=self.subparser.generate_code(),
            dontskip_parser_fn=self.dontskip_parser.gen_fn_name
        )

    def get_type(self):
        return self.subparser.get_type()

    def create_vars_after(self, start_pos):
        self.init_vars(self.subparser.pos_var, self.subparser.res_var)

    def _is_left_recursive(self, rule_name):
        return self.subparser._is_left_recursive(rule_name)


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

    def can_parse_token_node(self):
        return all(p.can_parse_token_node for p in self.parsers)

    @property
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
                res = ASTNodeType.common_ancestor(*types)
            else:
                typs = list(types)
                ref_type = typs[0]

                check_source_language(
                    all(t == ref_type for t in typs),
                    'Alternatives yield incompatible types: {}'.format(
                        ', '.join(sorted(t.dsl_name for t in typs))
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

    def discard(self):
        return all(p.discard() for p in self.parsers)


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
    Parser that scans a sequence of sub-parsers, remove tokens and ignored
    sub-parsers, and extract the only significant sub-result.

    If there are multiple significant sub-results, raises an error.
    """
    return _pick_impl(parsers)


def _pick_impl(parsers, no_checks=False):
    """
    Return a parser to scan a sequence of sub-parsers, removing tokens and
    ignored sub-parsers and extracting the only significant sub-result.

    :param bool no_checks: If left to false, check that only one parser in
        `parsers` generates an node. Otherwise, don't do this check.
    """
    location = extract_library_location()
    parsers = [resolve(p) for p in parsers if p]
    pick_parser_idx = -1
    for i, p in enumerate(parsers):
        if p.discard():
            continue
        with Context("", location):
            check_source_language(
                no_checks or pick_parser_idx == -1,
                "Pick parser can have only one sub-parser that is not a token",
                Severity.non_blocking_error
            )
        pick_parser_idx = i

    # If there is only one input parser, just return it instead of wrapping it
    # in several useless layers. We do this only now so that we nevertheless
    # check that it is a significant sub-result.
    if len(parsers) == 1:
        return parsers[0]

    if pick_parser_idx == -1:
        return _Row(*parsers)
    else:
        return _Extract(_Row(*parsers), pick_parser_idx)


class _Row(Parser):
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

        self.containing_transform = None
        """
        :type: _Transform
        If the containing parser is a Transform parser, and has no_backtrack to
        True, we want to track the progress of the Row. This variable is used
        to keep the containing transform parser if there is one.
        """

        self.progress_var = None
        """
        :type: VarDef

        If there is a containing_transform parser, this will be initialized to
        the progress var.
        """

    def discard(self):
        return all(p.discard() for p in self.parsers)

    @property
    def children(self):
        return self.parsers

    def get_type(self):
        # A _Row parser never yields a concrete result itself
        return None

    def create_vars_before(self):
        # We pass in a dummy object for res_var, because _Rows have no result
        self.init_vars(res_var=object())
        return self.pos_var

    def create_vars_after(self, pos_var):
        self.subresults = [p.res_var if not p.discard() else None
                           for p in self.parsers]

        # Create the progress variable if there is a containing transform in
        # no_backtrack mode.
        if (self.containing_transform
                and self.containing_transform.no_backtrack):
            self.progress_var = VarDef('row_progress', T.Int)

    def generate_code(self):
        return self.render('row_code_ada', exit_label=gen_name("Exit_Row"))


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

        :param ASTNodeType list_cls: If provided, it must be a
            ASTNodeType.list subtype to be used for the result of this
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

        sep = opts.pop('sep', None)
        self.sep = resolve(sep) if sep else None
        self.empty_valid = opts.pop('empty_valid', False)
        self.list_cls = opts.pop('list_cls', None)

        if opts:
            raise TypeError('unexpected keyword arguments: {}'.format(
                ', '.join(opts)))

    @property
    def children(self):
        return keep([self.parser, self.sep])

    def get_type(self):
        with self.diagnostic_context:
            if self.list_cls:
                ret = resolve_type(self.list_cls)
                check_source_language(
                    ret.is_list_type,
                    'Invalid list type for List parser: {}. '
                    'Not a list type'.format(ret.dsl_name)
                )
                return ret
            else:
                item_type = self.parser.get_type()
                check_source_language(
                    item_type.is_ast_node,
                    'List parsers only accept subparsers that yield AST nodes'
                    ' ({} provided here)'.format(item_type.dsl_name)
                )
                return item_type.list

    def compute_fields_types(self):
        Parser.compute_fields_types(self)

        typ = self.get_type()
        with self.diagnostic_context:
            check_source_language(
                not typ.abstract,
                'Please provide a concrete ASTnode subclass as list_cls'
                ' ({} is abstract)'.format(typ.dsl_name)
            )

    def create_vars_before(self):
        self.cpos = VarDef("lst_cpos", T.Token)
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

    def __init__(self, *parsers):
        """
        Create a parser that matches `parsers` if possible or matches an empty
        sequence otherwise.
        """
        Parser.__init__(self)

        self._booleanize = None
        """
        If it is an EnumNode subclass with qualifier set to True, then the
        result is booleanized into the corresponding two alternatives.
        Otherwise, must be None.

        :type: DSLType|CompiledType|None
        """

        self._is_error = False
        self.contains_anonymous_row = bool(parsers)
        self.parser = Pick(*parsers)

    def discard(self):
        return self._booleanize is None and self.parser.discard()

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

    def as_bool(self, dest):
        """
        Return the self parser, modified to return `dest` nodes rather than the
        sub-parser result. `dest` must be a qualifier EnumNode: the parser
        result will be the "true" qualifier if the parse was successful, and
        the "false" qualifier otherwise.

        This is typically useful to store specific tokens as attributes,
        for example in Ada, you'll mark a subprogram as overriding with the
        "overriding" keyword, and we want to store that in the tree as a
        boolean attribute, so we'll use::

            Opt("overriding").as_bool(OverridingQualifier)

        :param langkit.dsl.EnumNode dest: EnumNode subclass with qualifier set
            to True. The result will be booleanized using this enum type.

        :rtype: Opt
        """
        from langkit.dsl import EnumNode

        return copy_with(self, _booleanize=assert_type(dest, EnumNode))

    @property
    def booleanized_type(self):
        """
        For parsers that return a boolean, return the actual result type to use
        in code generation.

        :rtype: CompiledType
        """
        assert self._booleanize
        return resolve_type(self._booleanize)

    @property
    def children(self):
        return [self.parser]

    def get_type(self):
        if self._booleanize is None:
            return self.parser.get_type()
        else:
            assert self._booleanize._type
            return resolve_type(self._booleanize._type)

    def create_vars_after(self, start_pos):
        self.init_vars(
            self.parser.pos_var,
            res_var=None if self._booleanize else self.parser.res_var
        )

    def generate_code(self):
        return self.render('opt_code_ada')


class _Extract(Parser):
    """
    Wrapper parser used to discard everything from a _Row parser except a
    single field in it.
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
        :param _Row parser: The parser that will serve as target for
            extract operation.
        :param int index: The index you want to extract from the row.
        """
        Parser.__init__(self)
        self.parser = parser
        self.index = index
        assert isinstance(self.parser, _Row)

    @property
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

    @property
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

    @property
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


class _Transform(Parser):
    """
    Wrapper parser for a _Row parser used to instantiate an AST node.
    """

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self):
        return "Transform({0}, {1})".format(self.parser, node_name(self.typ))

    def __init__(self, parser, typ):
        """
        Create a _Transform parser wrapping `parser` and that instantiates AST
        nodes whose type is `typ`.
        """
        from langkit.dsl import ASTNode

        assert isinstance(parser, _Row)

        Parser.__init__(self)
        assert (issubtype(typ, ASTNode)
                or isinstance(typ, T.Defer)
                or typ.is_ast_node)

        self.parser = parser
        self.parser.containing_transform = self
        self.typ = typ
        self.has_failed_var = None
        """
        :type: VarDef

        If this transform is in a no_backtrack hierarchy, we will use this var
        to keep track of whether the transform has failed or not.
        """

    @property
    def children(self):
        return [self.parser]

    def get_type(self):
        return resolve_type(self.typ)

    def compute_fields_types(self):
        typ = self.get_type()

        # Sub-parsers for Token nodes must parse exactly one token
        if typ.is_token_node:
            check_source_language(
                len(self.parser.parsers) == 1,
                'Building {} requires a single input token (got {} subparsers)'
                .format(typ.dsl_name, self.parser)
            )
            check_source_language(
                self.parser.parsers[0].can_parse_token_node,
                'Building {} requires a single input token (got {})'.format(
                    typ.dsl_name, self.parser
                )
            )
            fields_types = []

        # Gather field types that come from all child parsers
        elif isinstance(self.parser, _Row):
            # There are multiple fields for _Row parsers
            fields_types = [parser.get_type()
                            for parser in self.parser.parsers
                            if not parser.discard()]
        elif isinstance(self.parser, _Token):
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
        nb_fields = len(typ.get_parse_fields())
        check_source_language(
            nb_transform_values == nb_fields,
            'Transform parser generates {} values, but {} has {} fields'
            .format(nb_transform_values, typ.dsl_name, nb_fields)
        )

        # Propagate types from self to destination node's fields
        typ.set_types(fields_types)

        # Handle sub-parsers
        Parser.compute_fields_types(self)

    def create_vars_after(self, start_pos):
        self.init_vars(self.parser.pos_var)
        if self.no_backtrack:
            self.has_failed_var = VarDef('transform_has_failed', T.Bool)

    def generate_code(self):
        return self.render(
            'transform_code_ada',
            args=(keep(self.parser.subresults)
                  if isinstance(self.parser, _Row) else [self.parser.res_var]),
        )


class Null(Parser):
    """
    Parser that matches the empty sequence and that yields no AST node.
    """

    def __init__(self, result_type):
        """
        Create a new Null parser.  `result_type` is either a CompiledType
        instance that defines what nullexpr this parser returns, either a
        Parser subclass' instance.  In the latter case, this parser will return
        the same type as the other parser.
        """
        Parser.__init__(self)
        self.typ = result_type

    @property
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
        return (self.typ.get_type()
                if isinstance(self.typ, Parser) else
                resolve_type(self.typ))


_ = Discard


class Predicate(Parser):
    """
    This composite parser takes as parameters a sub parser and a property
    reference that will be used as predicate, and will test the predicate on
    the node resulting of parsing. If true, it will return the node, else, it
    will error.
    """

    def __init__(self, parser, property_ref):
        """
        :param Parser parser: Sub-parser whose result is the predicate input.
        :param PropertyDef property_ref: Property to use as the predicate.
        """
        Parser.__init__(self)

        self.parser = resolve(parser)
        self.property_ref = property_ref

    def _is_left_recursive(self, rule_name):
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self):
        return 'Predicate({}, {})'.format(
            self.parser,
            (self.property_ref.label
             if isinstance(self.property_ref, T.Defer) else
             self.property_ref.qualname)
        )

    def create_vars_after(self, start_pos):
        self.init_vars()

    @property
    def children(self):
        return [self.parser]

    def get_type(self):
        return self.parser.get_type()

    def generate_code(self):
        self.property_ref = resolve_property(self.property_ref)

        check_source_language(
            self.property_ref.type.matches(T.Bool),
            "Property passed as predicate to Predicate parser must return"
            " a boolean"
        )

        check_source_language(
            self.property_ref.struct.matches(self.parser.get_type()),
            "Property passed as predicate to Predicate parser must take a node"
            " with the type of the sub-parser"
        )

        return self.render('predicate_code_ada')


class NoBacktrack(Parser):
    """
    An instance of this parser will indicate that no backtrack should happen in
    the parser hierarchy from now on. The semantics are similar to those of the
    cut operator, however, instead of using it to fail early and optimize
    performance, in this case we use it to provide error recovery.

    NOTE: It could eventually be used in a special "no error recovery" mode to
    fail early, if this is ever needed.
    """

    def discard(self):
        return True

    @property
    def children(self):
        return []

    def _is_left_recursive(self, rule_name):
        return False

    def __repr__(self):
        return "NoBacktrack"

    def create_vars_after(self, start_pos):
        self.pos_var = start_pos

    def generate_code(self):
        # Generated code only consists of setting the no_backtrack variable to
        # True, so that other parsers know that from now on they should not
        # backtrack.
        return "{} := True;".format(self.no_backtrack)

    def get_type(self):
        # A NoBacktrack parser never yields a concrete result itself
        return None


def node_name(node):
    from langkit.dsl import ASTNode

    if isinstance(node, T.Defer):
        node = node.get()

    if issubtype(node, ASTNode):
        return node._name.camel

    assert isinstance(node, ASTNodeType), (
        'Unexpected node type: {}'.format(repr(node))
    )
    return node.dsl_name
