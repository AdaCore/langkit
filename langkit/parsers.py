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

from __future__ import annotations

from collections import OrderedDict
from contextlib import contextmanager
import difflib
from funcy import keep
import inspect
from itertools import count
from typing import (
    Any, Callable, ContextManager, Dict, Iterator, List as _List, Optional,
    Sequence, Set, TYPE_CHECKING, Tuple, Type, Union
)

import funcy

from langkit import names
from langkit.common import gen_name
from langkit.compile_context import CompileCtx, get_context
from langkit.compiled_types import (
    ASTNodeType, CompiledType, Field, T, TokenType, TypeRepo, resolve_type
)
from langkit.diagnostics import (
    Location, Severity, check_source_language, diagnostic_context,
    extract_library_location
)
from langkit.expressions import PropertyDef, resolve_property
from langkit.lexer import Action, Literal, TokenAction, WithSymbol
from langkit.utils import copy_with, issubtype, type_check_instance
from langkit.utils.types import TypeSet


if TYPE_CHECKING:
    import liblktlang as L

    from langkit.dsl import ASTNode


def var_context() -> _List[VarDef]:
    """
    Returns the var context for the current parser.
    """
    return get_context().parsers_varcontext_stack[-1]


@contextmanager
def add_var_context() -> Iterator[_List[VarDef]]:
    """
    Context manager that will push a variable context into the stack when
    compiling a parser.
    """
    vc: _List[VarDef] = []
    get_context().parsers_varcontext_stack.append(vc)
    yield vc
    get_context().parsers_varcontext_stack.pop()


class VarDef:
    """
    Holder object for a variable in parsers. Creating an instance of vardef
    requires a context to exist already.
    """

    def __init__(self,
                 base_name: str,
                 type: Union[str, CompiledType],
                 create: bool = True,
                 reinit: bool = False):
        """
        Creates a VarDef.

        :param base_name: The string used as base for the name of the variable.
            A number will be appended in order to make the variable unique.

        :param type: The type of the variable.

        :param create: If true, the variable will be automatically added to the
            current variable context when created, and an unique name will be
            generated. If false, it is considered that the definition is done
            in the template, so the name used is the name passed, and the
            variable won't be added to the context.

        :param reinit: If true, the variable will be reinitialized to null expr
            if variable occurs in a left recursive parser, and left recursion
            is triggered.
        """

        self.type = type
        self.reinit = reinit

        # Add this variable to the current var context
        if create:
            self.name = gen_name(base_name)
            var_context().append(self)
        else:
            self.name = names.Name.from_lower(base_name)

    def __getitem__(self,
                    i: int) -> Union[names.Name, Union[str, CompiledType]]:
        """
        Helper, will allow users to destructures var defs into name and type,
        like::

            name, type = var

        Which in turns will allow iteration on the var context like::

            for name, type in var_context:
                ...
        """
        if i == 0:
            return self.name
        elif i == 1:
            return self.type
        else:
            raise IndexError

    def __str__(self) -> str:
        """
        Helper, expand var defs to their name when converted to strings,
        allows easy use of VarDef instances in code generation.
        """
        return str(self.name)


class NoVarDef:
    """
    Substitute for ``VarDef`` instances to mean "no variable" when ``None``
    already means "use the default".
    """
    pass


class GeneratedParser:
    """
    Simple holder for generated parsers.
    """

    def __init__(self, name: names.Name, spec: str, body: str):
        self.name = name
        self.spec = spec
        self.body = body


@CompileCtx.register_template_extensions
def template_extensions(ctx: CompileCtx) -> Dict[str, Any]:
    from langkit.unparsers import (
        ListNodeUnparser, RegularNodeUnparser, TokenNodeUnparser
    )
    return {
        'is_tok':       type_check_instance(_Token),
        'is_row':       type_check_instance(_Row),
        'is_dontskip':  type_check_instance(DontSkip),
        'is_defer':     type_check_instance(Defer),
        'is_transform': type_check_instance(_Transform),
        'is_list':      type_check_instance(List),
        'is_opt':       type_check_instance(Opt),
        'is_null':      type_check_instance(Null),
        'is_extract':   type_check_instance(_Extract),
        'is_class':     inspect.isclass,
        'is_regular_node_unparser': type_check_instance(RegularNodeUnparser),
        'is_list_node_unparser': type_check_instance(ListNodeUnparser),
        'is_token_node_unparser': type_check_instance(TokenNodeUnparser),
    }


def resolve(parser: Union[Parser, str, TokenAction]) -> Parser:
    """
    Resolve or wrap the argument to return a ``Parser`` instance.
    """
    if isinstance(parser, Parser):
        return parser
    elif isinstance(parser, str):
        return _Token(parser)
    elif isinstance(parser, TokenAction):
        return _Token(parser)
    else:
        raise Exception("Cannot resolve parser {}".format(parser))


def reject_abstract(node: ASTNodeType) -> None:
    check_source_language(not node.abstract,
                          'Parsers cannot create abstract nodes')


def reject_synthetic(node: ASTNodeType) -> None:
    check_source_language(not node.synthetic,
                          'Parsers cannot create synthetic nodes')


def reject_error_node(node: ASTNodeType) -> None:
    check_source_language(not node.is_error_node,
                          'Only Skip parsers can create error nodes')


class Grammar:
    """
    Holder for parsing rules.

    Parsing rules can be added incrementally while referencing each other: this
    class will automatically resolve forward references when needed.
    """

    def __init__(self,
                 main_rule_name: str,
                 extra_entry_points: Optional[Set[str]] = None,
                 location: Optional[Location] = None):
        """
        :param main_rule_name: Name of the main parsing rule.
        :param entry_points: Set of rule names for parsing rules that are entry
            points. Entry points are used as "used roots" for unused rules
            detection. The main rule is automatically considered as an entry
            point.
        """

        self.rules: Dict[str, Parser] = {}
        """
        For each parsing rule, associate its name to its parser.
        """

        self.main_rule_name = main_rule_name
        self.entry_points = extra_entry_points or set()
        self.entry_points.add(main_rule_name)

        self.user_defined_rules: _List[str]
        """
        List of rules names defined by the language spec.
        """

        self.user_defined_rules_indexes: Dict[str, int]
        """
        Reverse mapping for ``user_defined_rules``.
        """

        self.user_defined_rules_docs: Dict[str, str] = {}
        """
        Mapping from rules defined by the language spec to the corresponding
        documentations.
        """

        self.location: Optional[Location] = (
            location or extract_library_location()
        )

        self._all_lkt_rules: Dict[str, Tuple[L.Doc, L.GrammarExpr]] = (
            OrderedDict()
        )
        """
        If we loaded a Lkt unit, mapping of all grammar rules it contains.
        """

    def context(self) -> ContextManager[None]:
        return diagnostic_context(self.location)

    def _add_rule(self, name: str, parser: Parser, doc: str = "") -> None:
        """
        Add a rule to the grammar. The input parser is expected to have its
        location properly set at this point.

        :param name: Name for the new parsing rule.
        :param parser: Root parser for this rule.
        :param doc: Documentation for this rule.
        """
        parser.set_name(names.Name.from_lower(name))
        parser.set_grammar(self)
        parser.is_root = True

        with diagnostic_context(parser.location):
            check_source_language(
                name not in self.rules,
                "Rule '{}' is already present in the grammar".format(name)
            )

        self.rules[name] = parser

        # If we got a non-empty doc, use it. Otherwise, leave the possibly
        # predefined one.
        if doc:
            self.user_defined_rules_docs[name] = doc
        else:
            self.user_defined_rules_docs.setdefault(name, doc)

    def add_rules(self, **kwargs: Parser) -> None:
        """
        Add rules to the grammar.  The keyword arguments will provide a name to
        rules.

        :param kwargs: The rules to add to the grammar.
        """
        import ast

        _loc = extract_library_location()
        assert _loc is not None
        loc: Location = _loc

        class GetTheCall(ast.NodeVisitor):
            """
            Helper visitor that will get the corresponding add_rule call in the
            source, so that we're then able to extract the precise line where
            each rule is added.
            """
            def __init__(self) -> None:
                self.the_call: Optional[ast.Call] = None

            def visit_Call(self, call: ast.Call) -> None:
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

            assert the_call.the_call is not None

            # We're gonna use the keyword arguments to find back the precise
            # line where the rule was declared.
            keywords = {kw.arg: kw.value for kw in the_call.the_call.keywords}

        for name, rule in kwargs.items():
            rule = resolve(rule)
            if loc and name in keywords:
                rule.set_location(Location(loc.file, keywords[name].lineno))
            self._add_rule(name, rule)

    def get_rule(self, rule_name: str) -> Parser:
        """
        Helper to return the rule corresponding to rule_name. The benefit of
        using this helper is that it will raise a helpful error diagnostic.

        :param rule_name: Name of the rule to get.
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

    def rule_resolver(self, rule_name: str) -> Callable[[], Parser]:
        """
        Return a callable that returns the rule designated by ``rule_name``.
        """
        return lambda: self.get_rule(rule_name)

    def __getattr__(self, rule_name: str) -> Defer:
        """
        Build and return a Defer parser that references the above rule.

        :param rule_name: The name of the rule.
        """
        return Defer(rule_name, self.rule_resolver(rule_name))

    def get_unreferenced_rules(self) -> Set[str]:
        """
        Return a set of names for all rules that are not transitively
        referenced by entry points.
        """
        # We'll first build the set of rules that are referenced, then we'll
        # know the ones not referenced.
        referenced_rules: Set[str] = set()

        rule_stack: _List[Tuple[str, Parser]] = [
            (name, self.get_rule(name))
            for name in self.entry_points
        ]
        """
        List of couples names/parser for the rules still to visit.

        We use a stack to visit the entry points closures avoiding too deep
        recursion.
        """

        def visit_parser(parser: Parser) -> None:
            """
            Visit all subparsers in "parser" and call "visit_rule" for Defer
            parsers.

            :param parser: Parser to visit.
            """
            if (
                isinstance(parser, Defer)
                and parser.name not in referenced_rules
            ):
                with parser.diagnostic_context:
                    rule_stack.append(
                        (parser.name, self.get_rule(parser.name))
                    )

            for sub_parser in parser.children:
                visit_parser(sub_parser)

        def visit_rule(rule_name: str, parser: Parser) -> None:
            """
            Register "rule_name" as referenced and call "visit_parser" on the
            root parser that implements it. Do nothing if "rule_name" is
            already registered to avoid infinite recursion.

            :param rule_name: Name for the rule to visit.
            :param parser: Parser to visit.
            """
            if rule_name in referenced_rules:
                return
            referenced_rules.add(rule_name)
            visit_parser(parser)

        # The following will fill "referenced_rules" thanks to recursion
        while rule_stack:
            visit_rule(*rule_stack.pop())

        return set(self.rules) - referenced_rules

    def compute_user_defined_rules(self, context: CompileCtx) -> None:
        """
        Compute the list of rule names defined by the user.
        """
        self.user_defined_rules = []
        self.user_defined_rules_indexes = {}
        for rule, parser in self.rules.items():
            if not parser.is_dont_skip_parser:
                self.user_defined_rules.append(rule)
                self.user_defined_rules_indexes[rule] = len(
                    self.user_defined_rules
                )

    def warn_unreferenced_parsing_rules(self, context: CompileCtx) -> None:
        """
        Emit a warning for unreferenced parsing rules.
        """
        unreferenced_rules = self.get_unreferenced_rules()

        check_source_language(
            not unreferenced_rules, "The following parsing rules are not "
            "used: {}".format(", ".join(sorted(unreferenced_rules))),
            severity=Severity.warning
        )

    def check_entry_points(self, context: CompileCtx) -> None:
        """
        Emit an error if any of the entry points are missing.
        """
        with self.context():
            for name in sorted(self.entry_points):
                if not self.rules.get(name, None):
                    close_matches = difflib.get_close_matches(
                        name, self.rules.keys()
                    )

                    check_source_language(
                        False,
                        'Invalid rule name specified for main rule: "{}". '
                        '{}'.format(
                            name,
                            'Did you mean "{}"?'.format(close_matches[0])
                            if close_matches else ""
                        )
                    )


class Parser:
    """
    Base class for parsers building blocks.
    """

    # Hack: in order to preserve grammar rule declarations order from the DSL
    # to the concrete syntax, generate increasing unique IDs for parser
    # objects.
    _counter = iter(count(0))

    def __init__(self, location: Optional[Location] = None):
        self._id = next(self._counter)

        # Get the location of the place where this parser is created. This will
        # likely be overriden in Grammar.add_rules with a more precise
        # location if we can find the keyword argument in Python source code,
        # but if it is not, we have a degraded more.
        self.location = location or extract_library_location()

        self._mod = None
        self.gen_fn_name = gen_name(self.base_name)
        self.grammar: Optional[Grammar] = None
        self.is_root = False
        self._name: Optional[names.Name] = None
        self.no_backtrack: Optional[VarDef] = None
        """
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

        self._type_computed = False
        self._type: Optional[CompiledType] = None
        """
        Type that this parser creates (see the `get_type` method).

        It is computed during the type inference pass and, from there, must be
        accessed using the "type" property.
        """

        self.pos_var: Optional[VarDef] = None
        """
        Variable to contain a token index (T.Token) during code generation.

        Before this parser runs, this contains the index of the first token to
        analyze. After it has run, it contains the next index after the last
        token that was consumed by this parser.
        """

        self.res_var: Optional[VarDef] = None
        """
        Variable to contain the parser result (self.type) during code
        generation.
        """

    def traverse_create_vars(self, start_pos: VarDef) -> None:
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

    def traverse_dontskip(self, grammar: Grammar) -> None:
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
            root_node = get_context().root_grammar_class
            assert root_node is not None
            parsers: _List[Parser] = [Null(root_node)]
            parsers.extend(self.dontskip_parsers)
            self.dontskip_parser = _pick_impl(
                parsers, True, location=self.location
            )
            self.dontskip_parser.is_dont_skip_parser = True

            # Add a named rule for the the DontSkip parsers. Don't forget to
            # compile it (compute their types).
            grammar._add_rule(gen_name('dontskip_{}'.format(self.name)).lower,
                              self.dontskip_parser)
            self.dontskip_parser.compute_types()
            self.dontskip_parser.freeze_types()

        for c in self.children:
            c.traverse_dontskip(grammar)

    def traverse_nobacktrack(
        self,
        nobt: Optional[VarDef] = None
    ) -> Optional[VarDef]:
        """
        This method will traverse the parser hierarchy and set the no_backtrack
        variable if necessary, indicating which parsers should not backtrack.
        """
        if isinstance(self, Cut):
            # Do not create a new variable for consecutive Cuts

            if nobt:
                self.no_backtrack = nobt
            else:
                self.no_backtrack = VarDef('nobt', T.Bool, reinit=True)

        for c in self.children:
            nobt = c.traverse_nobacktrack(self.no_backtrack)

            # Or and Opt parsers are stop points for Cut:
            #
            # * For Or(A, B, ...) parsers, the effect of a Cut in A/B/... must
            #   stop when parsing returns from A/B/..., so we do not want the
            #   no_backtrack variable to be propagated from A to B, etc. and
            #   from A/B/... to the Or parser itself.
            #
            # * For Parser(A, Opt(B), Opt(C), ...) parsers, the effect of a Cut
            #   in A/... must stop when parsing B/C, so we do not want the
            #   no_backtrack variable to be propagated from A/... to B/C. On
            #   the other hand, a Cut in B should be propagated to the Parser
            #   itself, which includes A/... parsers, but not C (i.e. the
            #   effect of a Cut in B or C must not affect C or B, respectively,
            #   but only their parent parser Parser).

            if nobt and not isinstance(self, Or) and not isinstance(c, Opt):
                self.no_backtrack = nobt

            # If c is an Opt parser that contains a Cut, the no_backtrack value
            # of c will be propagated to self: create a no_backtrack variable
            # in self to hold the propagated value if no Cut has been defined
            # at this point in self yet.

            if nobt and not self.no_backtrack and isinstance(c, Opt):
                self.no_backtrack = VarDef('nobt', T.Bool, reinit=True)

        return self.no_backtrack

    def create_vars_after(self, start_pos: VarDef) -> None:
        """
        This callback can be implemented by parser subclasses, to create
        variables needed for parsing. If the parser needs to use the start_pos
        variable for either pos_var or res_var, this callback is the way to do
        it.
        """
        pass  # no-code-coverage

    def create_vars_before(self) -> Optional[VarDef]:
        """
        This callback can be implemented by parser subclasses, to create
        variables needed for parsing. If the parser needs to override the
        position variable for its sub-parsers, it should override this callback
        and return the new position variable.
        """
        pass  # no-code-coverage

    @classmethod
    def parser_cls_name(cls) -> str:
        """
        Return a name for this parser class to be used for code generation.
        """
        return cls.__name__.strip('_')

    def init_vars(self,
                  pos_var: Optional[VarDef] = None,
                  res_var: Union[VarDef, NoVarDef, None] = None) -> None:
        """
        Set or create variables for code generation.

        :param pos_var: If provided, variable to use as `self.pos_var`.
            Otherwise, create a new VarDef instance.
        :param res_var: If provided, variable to use as `self.res_var`.
            Otherwise, create a new VarDef instance.
        """
        base_name = self.parser_cls_name().lower()

        self.pos_var = pos_var or VarDef('{}_pos'.format(base_name), T.Token)
        if res_var is None:
            assert self.type is not None
            self.res_var = VarDef('{}_res'.format(base_name), self.type)
        elif isinstance(res_var, NoVarDef):
            self.res_var = None
        else:
            assert isinstance(res_var, VarDef)
            self.res_var = res_var

    @property
    def error_repr(self) -> str:
        """
        Return a representation of the parser suitable for generated code's
        error messages. Default implementation is to just use __repr__.
        """
        return repr(self)

    @property
    def base_name(self) -> names.Name:
        """
        Return a simple name (names.Name instance) for this parser.

        The result is used as a base name for the generated function name.
        """
        return names.Name.from_camel(self.parser_cls_name() + 'Parse')

    @property
    def name(self) -> str:
        assert self._name is not None
        return self._name.lower

    def discard(self) -> bool:
        return False

    def __or__(self, other: Parser) -> Parser:
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

    def set_location(self, location: Location) -> None:
        """
        Set the source location where this parser is defined. This is useful
        for error reporting purposes.
        """
        self.location = location
        for c in self.children:
            c.set_location(self.location)

    @property
    def diagnostic_context(self) -> ContextManager[None]:
        """
        Helper that will return a diagnostic context manager with parameters
        set for the grammar definition.
        """
        return diagnostic_context(self.location)

    def set_grammar(self, grammar: Grammar) -> None:
        """
        Associate `grammar` to this parser and to all its children.

        :param Grammar grammar: The grammar instance.
        """
        for c in self.children:
            c.set_grammar(grammar)
        self.grammar = grammar

    def set_name(self, name: names.Name) -> None:
        """
        Rename this parser and all its children so that `name` is part of the
        corresponding function in the generated code.

        :param name: The name to include in the name of this parser tree.
        """
        for c in self.children:
            if not c._name and not isinstance(c, Defer):
                c.set_name(name)

        self._name = name
        self.gen_fn_name = gen_name(name + self.base_name)

    def is_left_recursive(self) -> bool:
        """Return whether this parser is left-recursive."""
        return self._is_left_recursive(self.name)

    def _is_left_recursive(self, rule_name: str) -> bool:
        """
        Private function used only by is_left_recursive, will explore the
        parser tree to verify whether the named parser with name rule_name is
        left recursive or not.
        """
        raise NotImplementedError()

    @property
    def can_parse_token_node(self) -> bool:
        """
        Return whether this parser can be used as a sub-parser to build token
        nodes.

        :rtype: bool
        """
        return False

    @property
    def children(self) -> _List[Parser]:
        """
        Parsers are combined to create new and more complex parsers.  They make
        up a parser tree.  Return a list of children for this parser.

        Subclasses must override this method.
        """
        raise NotImplementedError()

    def check_toplevel_rules(self) -> None:
        """
        Make sure that top-level grammar rules yield nodes.
        """
        check_source_language(self.type is not None and self.type.is_ast_node,
                              'Grammar rules must yield a node')

    def compute_types(self) -> None:
        """
        Infer parse fields from this parser tree.
        """
        self._eval_type()
        for child in self.children:
            child.compute_types()

    def freeze_types(self) -> None:
        """
        Initialize `self.type` with the result of the `compute_types` pass.
        """
        self.type = self._eval_type()
        for child in self.children:
            child.freeze_types()

    def compile(self) -> None:
        """
        Compile this parser tree.

        Assuming that type inference on node parse fields already run, this
        performs general validation on parsers.
        """
        self._compile()
        for child in self.children:
            child.compile()

    def _compile(self) -> None:
        """
        Subclasses that need to perform specific actions during compilation
        must override this method to implement these actions.
        """
        pass

    def render_parser(self, context: CompileCtx) -> None:
        """
        Emit code for this parser as a function into the global context.

        :param langkit.compile_context.CompileCtx context: Global context.
        """
        assert self not in context.fns
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
                context.render_template('parsers/fn_profile_ada', t_env),
                context.render_template('parsers/fn_code_ada', t_env)
            ))

    @property
    def type(self) -> Optional[CompiledType]:
        """
        Type that this parser creates.

        This can be either an ASTNodeType subclass, the token type, or None if
        this parser does not yield a specific value itself (for instance Row
        parsers).
        """
        assert self._type_computed, 'Type not computed for {}'.format(self)
        return self._type

    @type.setter
    def type(self, typ: Optional[CompiledType]) -> None:
        assert not self._type_computed
        assert typ is None or isinstance(typ, (ASTNodeType, TokenType)), (
            'Invalid parser type: {}'.format(typ)
        )
        self._type_computed = True
        self._type = typ

    def _eval_type(self) -> Optional[CompiledType]:
        """
        Evaluate the type this parser creates.

        Subclasses must override this method. Because of the recursive nature
        of our type inference, this may return None when the type is still
        unknown.
        """
        raise NotImplementedError()

    @property
    def precise_types(self) -> TypeSet:
        """
        Return the precise set of nodes that this parser can return.

        This is valid only for parsers that return nodes.
        """
        assert self.type is not None
        assert self.type.is_ast_node, (
            'Node expected, {} found'.format(self.type.dsl_name))
        return self._precise_types()

    def _precise_types(self) -> TypeSet:
        """
        Implementation for precise_types. Relevant subclasses must override
        this.
        """
        raise NotImplementedError()

    @property
    def precise_element_types(self) -> TypeSet:
        """
        Return the precise set of nodes that the list nodes this parser return
        can contain.
        """
        assert (
            self.type is not None
            and self.type.is_ast_node
            and self.type.is_list_type
        )
        return self._precise_element_types()

    def _precise_element_types(self) -> TypeSet:
        """
        Implementation for precise_element_types. Relevant subclasses must
        override this.
        """
        raise NotImplementedError()

    def generate_code(self) -> str:
        """
        Return generated code for this parser into the global context.

        Subclasses must override this method.
        """
        raise NotImplementedError()

    def render(self, template_name: str, **kwargs: Any) -> str:
        """
        Shortcut for render for parsers, passing the parsers sub-path for
        templates, and self as "parser" in the template context.
        """
        return get_context().render_template(
            f"parsers/{template_name}", parser=self, **kwargs
        )

    @property
    def symbol_literals(self) -> Set[str]:
        """
        Return a list of strings for all symbol literals used by this parser
        and all its subparsers.

        Subclasses must override this method to actually include their own
        symbol literals.
        """
        result: Set[str] = set()
        for child in self.children:
            result.update(child.symbol_literals)
        return result

    def add_symbol_literals(self) -> None:
        """
        Register symbols literals used by this parser to "context".

        :type context: langkit.compile_context.CompileCtx
        """
        context = get_context()
        for sym in self.symbol_literals:
            context.add_symbol_literal(sym)

    def dont_skip(self, *parsers: Parser) -> DontSkip:
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
    def children(self) -> _List[Parser]:
        return []

    @property
    def error_repr(self) -> str:
        if self.val.matcher:
            assert isinstance(self.val.matcher, Literal)
            return self.val.matcher.to_match
        else:
            return str(self.val)

    def __repr__(self) -> str:
        return "Token({0})".format(repr(self._val))

    def discard(self) -> bool:
        return True

    def _is_left_recursive(self, rule_name: str) -> bool:
        return False

    def __init__(self,
                 val: Union[str, TokenAction],
                 match_text: str = "",
                 location: Optional[Location] = None):
        """
        Create a parser that matches a specific token.

        :param val: Either a reference to a TokenAction that is part of your
            lexer definition, either a string that will be used to find back
            the proper TokenAction.

        :param match_text: If val is a WithSymbol token action, allows to
            specify the exact text that should be matched by this parser.
        """
        assert isinstance(match_text, str)
        Parser.__init__(self, location=location)

        self._val: Union[str, Action] = val

        self._original_string: Optional[str] = None
        """
        Keep the original token string that was used to determine the token we
        want to parse.
        """

        self.match_text = match_text

        check_source_language(
            not self.match_text or isinstance(self._val, WithSymbol),
            "Tok matcher has match text, but val is not a WithSymbol instance,"
            " got {} instead".format(val)
        )

    @property
    def can_parse_token_node(self) -> bool:
        return True

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars()

    def _eval_type(self) -> CompiledType:
        return T.Token

    def _compile(self) -> None:
        # Resolve the token action associated with this parser
        if isinstance(self._val, str):
            self._original_string = self._val
            lexer = get_context().lexer
            assert lexer is not None
            self._val = lexer.get_token(self._val)
        else:
            check_source_language(isinstance(self._val, TokenAction),
                                  'Invalid token: {}'.format(self._val))

    @property
    def val(self) -> TokenAction:
        """
        Return the token action associated with this Tok parser.
        """
        # If this fails, it means we are trying to get the token action before
        # it was resolved (i.e. before the Parser.compile pass). This is
        # probably a bug in the compilation pipeline.
        assert isinstance(self._val, TokenAction)
        return self._val

    def generate_code(self) -> str:
        return self.render('tok_code_ada', token_kind=self.val.ada_name)

    @property
    def matches_symbol(self) -> bool:
        """
        Return whether this parser matches over a symbol's text.

        :rtype: bool
        """
        return bool(self.match_text) and isinstance(self.val, WithSymbol)

    @property
    def symbol_literals(self) -> Set[str]:
        return {self.match_text} if self.matches_symbol else set()


class Skip(Parser):
    """
    This recovery parser will skip any token and produce an error node from it,
    generating an error along the way.

    A common use of the ``Skip`` parser is to skip as many tokens as necessary
    for the parent parser to fallback into a state where it is able to parse
    something successfully.

    An example would be to use it in a list of statements::

        stmt=Or(
            call,
            assign,
            ...,
        )

        stmts=List(Or(stmt, Skip(ErrorDecl)))

    This way, if the parsing of any of the statements reaches an unexpected
    token that cannot be parsed by any of the statements, it will eventually be
    handled by the ``Skip`` parser, and produce an error decl for each
    unexpected token.

    Note that if you use that in a ``List`` parser like above, and that parser
    is itself in a parser, you will probably need to use the `DontSkip` parser
    to signal to the skip parser that it must *not* eat certain tokens. Let's
    develop the example above::

        stmt=Or(
            call,
            assign,
            ...,
        )

        stmts=List(Or(stmt, Skip(ErrorDecl)))

        function=Fn("function", name, "is", stmts, "end")

    In the example above, the stmts parser will eat *any* unexpected token,
    including the eventual "end" token that signals the end of the function,
    *before* the ``function`` parser has the opportunity to use it. To solve
    this, we'll use the `DontSkip` parser as such::

        function=Fn("function", name, "is", stmts.dont_skip("end"), "end")


    With this modification, the nested ``Skip`` parser will skip anything but
    the "end" token, making thus sure that the function can be parsed
    correctly.

    .. note:: As other things in Langkit parsers, those annotations could
        probably be statically derived from the grammar definition.
    """

    def __init__(self,
                 dest_node: Union[ASTNodeType, Type[ASTNode]],
                 location: Optional[Location] = None):
        """
        :param CompiledType dest_node: The error node to create.
        """
        Parser.__init__(self, location=location)
        self.dest_node = dest_node
        self.dest_node_parser = _Transform(_Row(), dest_node,
                                           force_error_node=True)

    def __repr__(self) -> str:
        return 'Skip({})'.format(node_name(self.dest_node))

    @property
    def children(self) -> _List[Parser]:
        return [self.dest_node_parser]

    def _eval_type(self) -> CompiledType:
        result = resolve_type(self.dest_node)
        check_source_language(
            result.is_error_node,
            'Skip parsers can only create error nodes'
        )
        return result

    def generate_code(self) -> str:
        return self.render('skip_code_ada', exit_label=gen_name("exit_or"))

    def _precise_types(self) -> TypeSet:
        return TypeSet([self.type])

    def _precise_element_types(self) -> TypeSet:
        assert self.type is not None
        return TypeSet([self.type.element_type])

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(res_var=self.dest_node_parser.res_var)
        self.dummy_node = VarDef('skip_dummy', T.root_node)

    def _is_left_recursive(self, rule_name: str) -> bool:
        return False


class DontSkip(Parser):
    """
    This is used in the following way::

        parser.dont_skip(other_parser)

    This means that in the scope of ``parser``, if a skip parser occurs, it
    will first run ``other_parser``, and if ``other_parser`` is successful,
    then skip will fail (eg. not skip anything).

    For more details about the use of this parser, check the documentation of
    ``Skip``.
    """

    def __init__(self,
                 subparser: Parser,
                 *dontskip_parsers: Parser,
                 **opts: Any):
        Parser.__init__(self, location=opts.pop('location', None))
        assert not opts
        self.subparser = resolve(subparser)
        self.dontskip_parsers = [resolve(sb) for sb in dontskip_parsers]

    def __repr__(self) -> str:
        return 'DontSkip({}, {})'.format(self.subparser, self.dontskip_parsers)

    @property
    def children(self) -> _List[Parser]:
        return [self.subparser]

    def _eval_type(self) -> Optional[CompiledType]:
        return self.subparser._eval_type()

    def generate_code(self) -> str:
        return """
        Parser.Private_Part.Dont_Skip.Append
          ({dontskip_parser_fn}'Access);
        {subparser_code}
        Parser.Private_Part.Dont_Skip.Delete_Last;
        """.format(
            subparser_code=self.subparser.generate_code(),
            dontskip_parser_fn=self.dontskip_parser.gen_fn_name
        )

    def _precise_types(self) -> TypeSet:
        return self.subparser.precise_types

    def _precise_element_types(self) -> TypeSet:
        return self.subparser.precise_element_types

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(self.subparser.pos_var, self.subparser.res_var)

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.subparser._is_left_recursive(rule_name)


class Or(Parser):
    """Parser that matches what the first sub-parser accepts."""

    def _is_left_recursive(self, rule_name: str) -> bool:
        return any(parser._is_left_recursive(rule_name)
                   for parser in self.parsers)

    def __repr__(self) -> str:
        return "Or({0})".format(", ".join(repr(m) for m in self.parsers))

    def __init__(self, *parsers: Union[Parser, str, TokenAction], **opts: Any):
        """
        Create a parser that matches any thing that the first parser in
        `parsers` accepts.
        """
        Parser.__init__(self, location=opts.pop('location', None))
        assert not opts
        self.parsers = [resolve(m) for m in parsers]

        # Typing resolution for this parser is a recursive process.  So first
        # we need to prevent infinite recursions (because of recursive
        # grammars)...
        self.is_processing_type = False

        # ... and we want to memoize the result.
        self.cached_type = None

    def can_parse_token_node(self) -> bool:
        return all(p.can_parse_token_node for p in self.parsers)

    @property
    def children(self) -> _List[Parser]:
        return self.parsers

    def _eval_type(self) -> Optional[CompiledType]:
        # Callers are already visiting this node, so we cannot return its type
        # right now.  Return None so that it doesn't contribute to type
        # resolution.
        if self.is_processing_type:
            return None

        try:
            self.is_processing_type = True
            types = set()
            for p in self.parsers:
                t = p._eval_type()
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

    def _precise_types(self) -> TypeSet:
        # We need protection from infinite recursion the same way get_type
        # does.
        if self.is_processing_type:
            return TypeSet()

        try:
            self.is_processing_type = True
            result = TypeSet()
            for p in self.parsers:
                result.update(p.precise_types)
            return result
        finally:
            self.is_processing_type = False

    def _precise_element_types(self) -> TypeSet:
        # We need protection from infinite recursion the same way get_type
        # does.
        if self.is_processing_type:
            return TypeSet()

        try:
            self.is_processing_type = True
            result = TypeSet()
            for p in self.parsers:
                result.update(p.precise_element_types)
            return result
        finally:
            self.is_processing_type = False

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars()

    def generate_code(self) -> str:
        return self.render('or_code_ada', exit_label=gen_name("exit_or"))

    def discard(self) -> bool:
        return all(p.discard() for p in self.parsers)


def always_make_progress(parser: Parser) -> bool:
    """
    Return whether `parser` cannot match an empty sequence of tokens.
    """
    if isinstance(parser, List):
        return not parser.empty_valid or always_make_progress(parser.parser)
    return not isinstance(parser, (Opt, Null))


def Pick(*parsers: Parser, **kwargs: Any) -> Parser:
    """
    Parser that scans a sequence of sub-parsers, remove tokens and ignored
    sub-parsers, and extract the only significant sub-result.

    If there are multiple significant sub-results, raises an error.
    """
    return _pick_impl(parsers, **kwargs)


def _pick_impl(parsers: Sequence[Parser],
               no_checks: bool = False,
               location: Optional[Location] = None) -> Parser:
    """
    Return a parser to scan a sequence of sub-parsers, removing tokens and
    ignored sub-parsers and extracting the only significant sub-result.

    :param no_checks: If left to false, check that only one parser in `parsers`
        generates an node. Otherwise, don't do this check.
    """
    location = location or extract_library_location()
    parsers = [resolve(p) for p in parsers if p]
    pick_parser_idx = -1
    for i, p in enumerate(parsers):
        if p.discard():
            continue
        with diagnostic_context(location):
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
        return _Row(*parsers, location=location)
    else:
        return _Extract(_Row(*parsers), pick_parser_idx, location=location)


class _Row(Parser):
    """
    Parser that matches a what sub-parsers match in sequence.
    """

    @property
    def error_repr(self) -> str:
        return " ".join(m.error_repr for m in self.parsers)

    def _is_left_recursive(self, rule_name: str) -> bool:
        for parser in self.parsers:
            res = parser._is_left_recursive(rule_name)
            if res:
                return True
            if always_make_progress(parser):
                break
        return False

    def __repr__(self) -> str:
        return "Row({0})".format(", ".join(repr(m) for m in self.parsers))

    def __init__(self, *parsers: Parser, **opts: Any):
        """
        Create a parser that matches the sequence of matches for all
        sub-parsers in `parsers`.

        If a parser is none it will be ignored. This allows to create
        programmatic helpers that generate rows more easily.

        :type parsers: list[Parser|types.Token|type]
        """
        Parser.__init__(self, location=opts.pop('location', None))
        assert not opts

        self.parsers = [resolve(m) for m in parsers if m]

        # The type this row returns is initialized either when assigning a
        # wrapper parser or when trying to get the type (though the get_type
        # method) while no wrapper has been assigned.
        self.typ: Optional[CompiledType] = None

        self.containing_transform: Optional[_Transform] = None
        """
        If the containing parser is a Transform parser, and has no_backtrack to
        True, we want to track the progress of the Row. This variable is used
        to keep the containing transform parser if there is one.
        """

        self.progress_var: Optional[VarDef] = None
        """
        If there is a containing_transform parser, this will be initialized to
        the progress var.
        """

        self.subresults: Optional[_List[Optional[VarDef]]] = None
        """
        Holder for code generation. List of couple variables holding the parser
        result for each sub-parser in this row.
        """

    def discard(self) -> bool:
        return all(p.discard() for p in self.parsers)

    @property
    def children(self) -> _List[Parser]:
        return self.parsers

    def _eval_type(self) -> Optional[CompiledType]:
        # A _Row parser never yields a concrete result itself
        return None

    def create_vars_before(self) -> Optional[VarDef]:
        # Do not create a variable for the result as rows have no result
        # themselves.
        self.init_vars(res_var=NoVarDef())
        return self.pos_var

    def create_vars_after(self, pos_var: VarDef) -> None:
        self.subresults = [p.res_var if not p.discard() else None
                           for p in self.parsers]

        # Create the progress variable if there is a containing transform in
        # no_backtrack mode.
        if (self.containing_transform
                and self.containing_transform.no_backtrack):
            self.progress_var = VarDef('row_progress', T.Int)

    def generate_code(self) -> str:
        return self.render('row_code_ada', exit_label=gen_name("exit_row"))


class List(Parser):
    """
    Parser that matches a list.  A sub-parser matches list items.
    """

    def _is_left_recursive(self, rule_name: str) -> bool:
        res = self.parser._is_left_recursive(rule_name)
        assert not (
            res and (self.empty_valid or not always_make_progress(self.parser))
        )
        return res

    def __repr__(self) -> str:
        return "List({0})".format(
            repr(self.parser) + (", sep={0}".format(self.sep)
                                 if self.sep else "")
        )

    def __init__(self, *parsers: Parser, **opts: Any):
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

        Parser.__init__(self, location=opts.pop('location', None))
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
    def children(self) -> _List[Parser]:
        return keep([self.parser, self.sep])

    def _eval_type(self) -> Optional[CompiledType]:
        with self.diagnostic_context:
            # Compute the type of list elements
            item_type = self.parser._eval_type()

            if self.list_cls:
                # If a specific list class is to be used, check that...
                result = resolve_type(self.list_cls)

                # It is not synthetic, nor an error node
                reject_synthetic(result)
                reject_error_node(result)

                # It is a list node
                check_source_language(
                    result.is_list_type,
                    'Invalid list type for List parser: {}.'
                    ' Not a list type'.format(result.dsl_name)
                )

                # If we already know the type that the sub-parser returns,
                # check that it fits in the requested list class.
                if item_type is not None:
                    check_source_language(
                        item_type.matches(result.element_type),
                        'Invalid list type for List parser: sub-parser'
                        ' produces {} nodes while {} accepts only {} nodes'
                        .format(item_type.dsl_name,
                                result.dsl_name,
                                result.element_type.dsl_name))

            else:
                assert isinstance(item_type, CompiledType)
                check_source_language(
                    item_type.is_ast_node,
                    'List parsers only accept subparsers that yield AST nodes'
                    ' ({} provided here)'.format(item_type.dsl_name)
                )
                assert isinstance(item_type, ASTNodeType)
                result = item_type.list

        result.add_list_element_parser(self.parser)
        return result

    def _precise_types(self) -> TypeSet:
        return TypeSet([self.type])

    def _precise_element_types(self) -> TypeSet:
        return self.parser.precise_types

    def _compile(self) -> None:
        # Ensure this list parser will build concrete list nodes.
        # TODO: we should be able to do this in _eval_type directly.
        with self.diagnostic_context:
            assert isinstance(self.type, ASTNodeType)
            check_source_language(
                not self.type.abstract,
                'Please provide a concrete ASTnode subclass as list_cls'
                ' ({} is abstract)'.format(self.type.dsl_name)
            )

    def create_vars_before(self) -> Optional[VarDef]:
        self.cpos = VarDef("lst_cpos", T.Token)
        self.tmplist = VarDef('tmp_list', 'Free_Parse_List')
        return self.cpos

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars()

    def generate_code(self) -> str:
        return self.render('list_code_ada')


class Opt(Parser):
    """
    Parser that matches something if possible or that matches an empty sequence
    otherwise.
    """

    @property
    def error_repr(self) -> str:
        return "[{}]".format(self.parser.error_repr)

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self) -> str:
        args = [str(self.parser)]
        if self._booleanize:
            args.append('to_bool={}'.format(self._booleanize))
        return "Opt({0})".format(', '.join(args))

    def __init__(self, *parsers: Parser, **opts: Any):
        """
        Create a parser that matches `parsers` if possible or matches an empty
        sequence otherwise.
        """
        Parser.__init__(self, location=opts.pop('location', None))

        self._booleanize = None
        """
        If it is an enum node with qualifier set to True, then the result is
        booleanized into the corresponding two alternatives. Otherwise, must be
        None.

        :type: DSLType|CompiledType|None
        """

        self._is_error = False
        self.contains_anonymous_row = bool(parsers)
        self.parser = Pick(*parsers)

    def discard(self) -> bool:
        return self._booleanize is None and self.parser.discard()

    def error(self) -> Parser:
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
        """
        return copy_with(self, _is_error=True)

    def as_bool(self, dest: Union[ASTNodeType, Type[ASTNode]]) -> Parser:
        """
        Return the self parser, modified to return `dest` nodes rather than the
        sub-parser result. `dest` must be a bool enum node: the parser
        result will be the "true" qualifier if the parse was successful, and
        the "false" qualifier otherwise.

        This is typically useful to store specific tokens as attributes,
        for example in Ada, you'll mark a subprogram as overriding with the
        "overriding" keyword, and we want to store that in the tree as a
        boolean attribute, so we'll use::

            Opt("overriding").as_bool(OverridingQualifier)

        :param dest: An enum node with qualifier set to
            True. The result will be booleanized using this enum node type.
        """
        assert (dest.is_enum_node
                if isinstance(dest, ASTNodeType)
                else dest._is_enum_node)
        return copy_with(self, _booleanize=dest)

    @property
    def booleanized_type(self) -> CompiledType:
        """
        For parsers that return a boolean, return the actual result type to use
        in code generation.
        """
        assert self._booleanize
        return resolve_type(self._booleanize)

    @property
    def children(self) -> _List[Parser]:
        return [self.parser]

    def _eval_type(self) -> Optional[CompiledType]:
        if self._booleanize is None:
            return self.parser._eval_type()
        else:
            result = resolve_type(self._booleanize)
            reject_synthetic(result)
            reject_error_node(result)
            return result

    def _precise_types(self) -> TypeSet:
        return (self.parser.precise_types
                if self._booleanize is None else
                TypeSet([self.type]))

    def _precise_element_types(self) -> TypeSet:
        # If we booleanize, then we're not definitely not building a list, so
        # asking for the element type makes no sense.
        assert self._booleanize is None
        return self.parser.precise_element_types

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(
            self.parser.pos_var,
            res_var=None if self._booleanize else self.parser.res_var
        )

    def generate_code(self) -> str:
        return self.render('opt_code_ada')


class _Extract(Parser):
    """
    Wrapper parser used to discard everything from a _Row parser except a
    single field in it.
    """

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.parser._is_left_recursive(rule_name)

    @property
    def error_repr(self) -> str:
        return self.parser.error_repr

    def __repr__(self) -> str:
        return "Extract({0}, {1})".format(self.parser, self.index)

    def __init__(self,
                 parser: Parser,
                 index: int,
                 location: Optional[Location] = None):
        """
        :param parser: The parser that will serve as target for extract
            operation.
        :param index: The index you want to extract from the row.
        """
        Parser.__init__(self, location=location)
        p = resolve(parser)
        assert isinstance(p, _Row)
        self.parser = p
        self.index = index

    @property
    def children(self) -> _List[Parser]:
        return [self.parser]

    def _eval_type(self) -> Optional[CompiledType]:
        return self.parser.parsers[self.index]._eval_type()

    def _precise_types(self) -> TypeSet:
        return self.parser.parsers[self.index].precise_types

    def _precise_element_types(self) -> TypeSet:
        return self.parser.parsers[self.index].precise_element_types

    def create_vars_after(self, start_pos: VarDef) -> None:
        assert self.parser.subresults is not None
        self.init_vars(
            self.parser.pos_var, self.parser.subresults[self.index]
        )

    def generate_code(self) -> str:
        return self.parser.generate_code()


class Discard(Parser):
    """
    Wrapper parser used to discard the match.
    """

    def discard(self) -> bool:
        return True

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self) -> str:
        return "Discard({0})".format(self.parser)

    def __init__(self, parser: Parser, location: Optional[Location] = None):
        Parser.__init__(self, location=location)

        parser = resolve(parser)
        self.parser = parser

    @property
    def children(self) -> _List[Parser]:
        return [self.parser]

    def _eval_type(self) -> Optional[CompiledType]:
        # Discard parsers return nothing
        return None

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(self.parser.pos_var, self.parser.res_var)

    def generate_code(self) -> str:
        return self.parser.generate_code()


class Defer(Parser):
    """
    Stub parser used to implement forward references.
    """

    @property
    def children(self) -> _List[Parser]:
        # We don't resolve the Defer's pointed parser here, because that would
        # transform the parser tree into a graph.
        return []

    @property
    def error_repr(self) -> str:
        return self.name

    @property
    def name(self) -> str:
        # Don't rely on `self.parser` since it may not be available right now
        # (that's why it is deferred in the first place).
        return self.rule_name

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.name == rule_name

    def __repr__(self) -> str:
        return "{0}".format(self.name)

    def __init__(self,
                 rule_name: str,
                 parser_fn: Callable[[], Parser],
                 location: Optional[Location] = None):
        """
        Create a stub parser.

        :param str rule_name: the name of the deferred parser (used for
            pretty-printing).
        :param callable parser_fn: must be a callable that returns the
            referenced parser.
        """
        Parser.__init__(self, location=location)
        self.rule_name = rule_name
        self.parser_fn = parser_fn
        self._parser: Optional[Parser] = None

    @property
    def parser(self) -> Parser:
        if self._parser is None:
            self._parser = self.parser_fn()
        return self._parser

    def _eval_type(self) -> Optional[CompiledType]:
        return self.parser._eval_type()

    def _precise_types(self) -> TypeSet:
        return self.parser.precise_types

    def _precise_element_types(self) -> TypeSet:
        return self.parser.precise_element_types

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars()

    def generate_code(self) -> str:
        # Generate a call to the function implementing the deferred parser
        return self.render('fn_call_ada')


class _Transform(Parser):
    """
    Wrapper parser for a _Row parser used to instantiate an AST node.
    """

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.parser._is_left_recursive(rule_name)

    def __repr__(self) -> str:
        return "Transform({0}, {1})".format(self.parser, node_name(self.typ))

    def __init__(self,
                 parser: Parser,
                 typ: Union[ASTNodeType, Type[ASTNode]],
                 force_error_node: bool = False,
                 location: Optional[Location] = None):
        """
        Create a _Transform parser wrapping `parser` and that instantiates AST
        nodes whose type is `typ`.

        :param force_error_node: Whether "typ" is an error node, which is
            forbidden for transform parsers from the language spec.
        """
        from langkit.dsl import ASTNode

        parser = resolve(parser)
        assert isinstance(parser, _Row)

        Parser.__init__(self, location=location)
        assert (issubtype(typ, ASTNode)
                or isinstance(typ, T.Defer)
                or (isinstance(typ, CompiledType) and typ.is_ast_node))

        self.parser = parser
        self.parser.containing_transform = self
        self.typ = typ
        self.force_error_node = force_error_node

        self.parse_fields: Optional[_List[Field]] = None
        """
        List of fields that this parser initializes in the result.
        """

        self.has_failed_var: Optional[VarDef] = None
        """
        If this transform is in a no_backtrack hierarchy, we will use this var
        to keep track of whether the transform has failed or not.
        """

        self.diagnostics_var: Optional[VarDef] = None
        """
        Variable used to track the number of diagnostics when starting to run
        this parser. Used to remove extra diagnostics in case this parser
        failed and returns a null node.
        """

        self.cached_type: Optional[CompiledType] = None

    @property
    def children(self) -> _List[Parser]:
        return [self.parser]

    def _precise_types(self) -> TypeSet:
        return TypeSet([self.type])

    # Transform cannot be used to create a list type, so it makes no sense to
    # ask its precise element types.

    @property
    def fields_parsers(self) -> _List[Parser]:
        """
        Return the list of parsers that return values for the fields in the
        node this parser creates.
        """
        typ = resolve_type(self.typ)

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
            return []

        # Gather field types that come from all child parsers
        elif isinstance(self.parser, _Row):
            # There are multiple fields for _Row parsers
            return [p for p in self.parser.parsers if not p.discard()]
        elif isinstance(self.parser, _Token):
            return []
        else:
            return [self.parser]

    def _eval_type(self) -> Optional[CompiledType]:
        if self.cached_type is not None:
            return self.cached_type

        result = resolve_type(self.typ)
        reject_abstract(result)
        reject_synthetic(result)
        if self.force_error_node:
            assert result.is_error_node
        else:
            reject_error_node(result)

        self.parse_fields = result.get_parse_fields(
            predicate=lambda f: not f.abstract and not f.null
        )
        assert isinstance(self.parse_fields, list)

        # Check that the number of values produced by self and the number of
        # fields in the destination node are the same.
        nb_transform_values = len(self.fields_parsers)
        nb_fields = len(self.parse_fields)
        check_source_language(
            nb_transform_values == nb_fields,
            'Transform parser gets {} values, but {} has {} fields'
            .format(nb_transform_values, result.dsl_name, nb_fields)
        )

        # Register this parser to the constructed type, which will propagate
        # field types.
        result.add_transform(self)

        self.cached_type = result
        return result

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(self.parser.pos_var)
        if self.no_backtrack:
            self.has_failed_var = VarDef('transform_has_failed', T.Bool)
        self.diagnostics_var = VarDef(
            "transform_diags", "Ada.Containers.Count_Type"
        )

    def generate_code(self) -> str:
        subparsers: _List[Tuple[Parser, VarDef]]
        if isinstance(self.parser, _Row):
            subparsers = funcy.lzip(self.parser.parsers,
                                    self.parser.subresults)
        else:
            subparsers = [(self.parser, self.parser.res_var)]

        # Remove subparsers that do not contribute to field creation
        subparsers = [(p, v) for p, v in subparsers if v]
        assert self.parse_fields is not None
        assert len(self.parse_fields) == len(subparsers)

        return self.render('transform_code_ada',
                           args=[(f, p, v) for f, (p, v)
                                 in zip(self.parse_fields, subparsers)])


class Null(Parser):
    """
    Parser that matches the empty sequence and that yields no AST node.
    """

    def __init__(self,
                 result_type: Union[ASTNodeType, Union[ASTNode]],
                 location: Optional[Location] = None):
        """
        Create a new Null parser.  `result_type` is either a CompiledType
        instance that defines what nullexpr this parser returns, either a
        Parser subclass' instance.  In the latter case, this parser will return
        the same type as the other parser.
        """
        Parser.__init__(self, location=location)
        self.type_or_parser = result_type

    @property
    def children(self) -> _List[Parser]:
        # We don't consider self.type_or_parser as a child since, if it is
        # parser, it will not be used for parsing, just for typing.
        return []

    def freeze_types(self) -> None:
        Parser.freeze_types(self)

        # Even though we don't consider the sub-parser as a children, we need
        # its types to be freezed for the precise types machinery.
        if isinstance(self.type_or_parser, Parser):
            self.type_or_parser.freeze_types()

    def _is_left_recursive(self, rule_name: str) -> bool:
        return False

    def __repr__(self) -> str:
        return "Null"

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(start_pos)

    def generate_code(self) -> str:
        return self.render('null_code_ada')

    def _eval_type(self) -> Optional[CompiledType]:
        if isinstance(self.type_or_parser, Parser):
            result = self.type_or_parser._eval_type()
        else:
            result = resolve_type(self.type_or_parser)
        reject_synthetic(result)
        reject_error_node(result)
        return result

    # This parser always return a null node, so it does not contribute to the
    # list of precise types.

    def _precise_types(self) -> TypeSet:
        return TypeSet([])

    def _precise_element_types(self) -> TypeSet:
        assert self.type is not None
        return TypeSet([])


_ = Discard


class Predicate(Parser):
    """
    This composite parser takes as parameters a sub parser and a property
    reference that will be used as predicate, and will test the predicate on
    the node resulting of parsing. If true, it will return the node, else, it
    will error.
    """

    def __init__(self,
                 parser: Parser,
                 property_ref: Union[TypeRepo.Defer, PropertyDef],
                 location: Optional[Location] = None):
        """
        :param parser: Sub-parser whose result is the predicate input.
        :param property_ref: Property to use as the predicate.
        """
        Parser.__init__(self, location=location)

        self.parser = resolve(parser)
        self.property_ref = property_ref

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.parser._is_left_recursive(rule_name)

    @property
    def property_name(self) -> str:
        """
        Informal representation for the property reference that this predicate
        references.
        """
        return (self.property_ref.label
                if isinstance(self.property_ref, T.Defer) else
                self.property_ref.qualname)

    def __repr__(self) -> str:
        return 'Predicate({}, {})'.format(self.parser, self.property_name)

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars()

    @property
    def children(self) -> _List[Parser]:
        return [self.parser]

    def _eval_type(self) -> Optional[CompiledType]:
        return self.parser._eval_type()

    def _precise_types(self) -> TypeSet:
        return self.parser.precise_types

    def _precise_element_types(self) -> TypeSet:
        return self.parser.precise_element_types

    def _compile(self) -> None:
        # Resolve the property reference and make sure it has the expected
        # signature: (parser-result-type) -> bool.
        self.property_ref = resolve_property(self.property_ref)
        assert isinstance(self.property_ref, PropertyDef)
        check_source_language(
            self.property_ref.type.matches(T.Bool),
            'Property passed as predicate to Predicate parser must return'
            ' a boolean')
        assert self.property_ref.struct is not None
        check_source_language(
            self.property_ref.struct.matches(self.parser.type),
            'Property passed as predicate to Predicate parser must take a node'
            ' with the type of the sub-parser')

    def generate_code(self) -> str:
        return self.render('predicate_code_ada')


class StopCut(Parser):
    """
    This parser is meant to be used in combination with the ``Cut`` parser,
    containing its effects, by making sure the parser backtracks, and
    cancelling eventual error messages that might have been emitted.

    * If parsing of ``StopCut``'s subparser is successful, nothing happens.

    * If parsing of ``StopCut``'s subparser fails because of a cut (so
      returning an incomplete node), then the result is discarded, and
      ``StopCut`` returns a null result and position, forcing the parent parser
      to backtrack.

    * If parsing of ``StopCut``'s subparser fails completely (so returning
      null), then we still reset eventual error messages that might have been
      emitted in nested ``Cut`` parsers.


    Here is an example of how to use the ``StopCut`` parser. Imagine you have
    the two following rules::

        def1=Def("def", Cut(), ..., "end")
        def2=Var("def", Cut(), ...)

    So far, we imagine it's fine to have cuts in each rule, because they're not
    used in a context where they can clash. But then at some point you add the
    following rule to your parser::

        block=Block("{", Or(def1, def2), "}")

    Then it doesn't work, because ``def2`` can't ever be parsed, and you will
    always get an error node out of ``def1`` in cases where you try to parse
    ``def2``. If you add a ``StopCut`` parser you can solve that problem::

        block=Block("{", Or(StopCut(def1), def2), "}")

    In this case, the parsing of ``def1`` will fail rather than return an error
    node, and so the ``Or`` will backtrack on ``def2``.
    """

    def __init__(self, parser: Parser, location: Optional[Location] = None):
        Parser.__init__(self, location=location)
        self.parser = resolve(parser)

    def __repr__(self) -> str:
        return f"StopCut({self.parser})"

    def _is_left_recursive(self, rule_name: str) -> bool:
        return self.parser._is_left_recursive(rule_name)

    @property
    def children(self) -> _List[Parser]:
        return [self.parser]

    def _eval_type(self) -> Optional[CompiledType]:
        return self.parser._eval_type()

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.init_vars(res_var=self.parser.res_var)

    def _precise_types(self) -> TypeSet:
        return self.parser.precise_types

    def _precise_element_types(self) -> TypeSet:
        return self.parser.precise_element_types

    def generate_code(self) -> str:
        return f"""
        declare
            Nb_Diags : constant Ada.Containers.Count_Type
              := Parser.Diagnostics.Length;
        begin
            {self.parser.generate_code()}

            if ({self.parser.res_var} /= null
                and then {self.parser.res_var}.Last_Attempted_Child /= -1)

               --  If the subparser has failed, we still want to reset the
               --  diagnostics, because parsing might have failed because of
               --  a cut.
               or else {self.parser.pos_var} = No_Token_Index
            then
                {self.pos_var} := No_Token_Index;
                Parser.Diagnostics.Set_Length (Nb_Diags);
            else
                {self.pos_var} := {self.parser.pos_var};
            end if;
        end;
        """


class Cut(Parser):
    """
    An instance of this parser will prevent backtracking from the point where
    the cut is inserted.

    Diagnostics will be emitted (so the resulting analysis unit will
    necessarily be considered as being erroneous), the parser will return an
    incomplete error node, *but will not return ``No_Token_Index``*, and hence
    parsing will continue from the next token.

    Note that in the general case, this behavior (parsing continuing), will
    just lead to an immediate failure, because the parser will start back from
    a point that makes no sense. But, combined with the use of ``Skip``
    parsers, this can allow the user to continue parsing by skipping every
    token until it finds something that makes sense.

    This parser is used to:

    1. Get better error messages. You are indicating the parser that if
       something fails past a certain point, it should not backtrack. That
       allows the parser to generate a specific error message for that branch,
       instead of a more generic error message later on.

       .. NOTE:: This is only half true for the moment. We can definitely
           further improve the quality of error messages Cuts produce.

    2. Get better error recovery. So far this is the primary reason the ``Cut``
       parser is used. Associated with other error recovery primitives in
       Langkit, such as the ``Skip`` parser, this will allow you to generate
       more precise incomplete/incorrect nodes.

    Here is an example of how to use the ``Cut`` parser. Let's take the example
    of a very simple rule like the following::

        fn=Fn("function", name, "is", stmts, "end")

    In this case, if we try to parse the input ``"function end"``, it will fail
    completely, and not produce any node.

    If we add a cut in the rule like so::

        fn=Fn("function", Cut(), name, "is", stmts, "end")

    Then, parsing the same input will produce an incomplete ``Fn`` node.
    Parsing will probably fail on the next token (``end``) anyway.

    If you combine that with the use of the ``Skip`` parser, you can get a
    parser that can recover errors::

        fn=Fn("function", Cut(), name, "is", stmts, "end")
        typ=Typ("type", Cut(), name, "is", ..., "end")
        decls=List(Or(fn, typ, Skip(ErrorDecl)))

    With this parser, you can parse incomplete code such as::


        type A             --  This will produce an incomplete node thanks to
                           --  the cut annotation, and parser will start back
                           --  from the "end" token.

        end                --  This token will be skipped thanks to the
                           --  ``Skip`` parser.

        function Foo is    --  This function decl will be parsed correctly
            print("lol")
        end

    Still in the perspective of better error recovery, a ``Cut`` parser is also
    allowed in an ``Opt`` parser in order to prevent backtracking even when an
    ``Opt`` parser fails. Here is an example of how to use the ``Cut`` parser
    in an ``Opt`` one::

        body=Body(Opt("scope", identifier), "begin", stmts_list, "end")

    In this case, if we try to parse the input ``"scope begin [stmts] end"``,
    it will fail because of the missing ``identifier`` field, the ``Opt``
    parser will backtrack and the ``scope`` keyword will report an error.
    Nevertheless, it can be improved thanks to a ``Cut``::

        body=Body(Opt("scope", Cut(), identifier), "begin", stmts_list, "end")

    Now, the parser will not backtrack and produce an incomplete node, taking
    into account the ``Opt`` part. The error will now concern the
    ``identifier`` field being absent instead of complaining about the
    ``scope`` keyword. This also means that on the simple input: ``"scope"``,
    the parser won't backtrack and produce an incomplete ``Body`` node.

    Note that the ``Cut`` parser only applies to the ``Opt`` parser it is
    defined in, therefore, the parser will backtrack on the following input:
    ``"begin end"``. Here, the parser will fail because of the missing
    ``stmts_list`` field. Several ``Cut`` parsers can be used to improve error
    recovery in that case. Rewriting the rule as::

        body=Body(Opt("scope", Cut(), identifier),
                  "begin", Cut(), stmts_list, "end")

    will allow the parser to properly parse the incomplete input, reporting the
    missing ``stmts_list`` field. Moreover, if no ``Cut`` is defined in the
    ``Opt`` parser::

        body=Body(Opt("scope", identifier), "begin", Cut(), stmts_list, "end")

    The ``Cut`` in the ``Body`` parser has no effect in the ``Opt`` part, which
    means that the following input: ``"scope begin end"``, will produce a
    parsing error and won't recover anything from the ``Opt`` parser: the
    ``identifier`` being absent, the ``Opt`` parser will fail and backtrack,
    the ``scope`` keyword will be reported as en error, and, the ``begin end``
    will be incompletely parsed (no backtrack because of the ``Cut``).
    """

    def discard(self) -> bool:
        return True

    @property
    def children(self) -> _List[Parser]:
        return []

    def _is_left_recursive(self, rule_name: str) -> bool:
        return False

    def __repr__(self) -> str:
        return "Cut"

    def create_vars_after(self, start_pos: VarDef) -> None:
        self.pos_var = start_pos

    def generate_code(self) -> str:
        # Generated code only consists of setting the no_backtrack variable to
        # True, so that other parsers know that from now on they should not
        # backtrack.
        return '{} := True;'.format(self.no_backtrack)

    def _eval_type(self) -> Optional[CompiledType]:
        # A Cut parser never yields a concrete result itself
        return None


def node_name(node: Union[TypeRepo.Defer, Type[ASTNode], ASTNodeType]) -> str:
    from langkit.dsl import ASTNode

    if isinstance(node, T.Defer):
        node = node.get()

    if issubtype(node, ASTNode):
        return node._name.camel

    assert isinstance(node, ASTNodeType), (
        'Unexpected node type: {}'.format(repr(node))
    )
    return node.dsl_name
