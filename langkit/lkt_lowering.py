"""
Module to gather the logic to lower Lkt syntax trees to Langkit internal data
structures.

Global architecture:

* In the constructor of CompileCtx, all Lkt units are loaded into the
  CompileCtx.lkt_units list. At this stage, compilation is aborted in case of
  lexing/parsing error.

* During the "lower_lkt" pass:

  * The "create_lexer" function below instantiates the langkit.lexer.Lexer
    class and populates it.

  * The "create_grammar" function below instantiates the
    langkit.parsers.Grammar class and populates it.

  * The "create_types" function below instantiates all CompiledType instances
    mentionned in the language spec.

The last step is the most complex one: type declarations refer to each other,
and this step includes the lowering of property expressions to abstract
expressions. The lowering of types goes as follows:

* The first pass looks at all top-level declarations (lexers, grammars and
  types) and registers them by name in the root scope.

* At this point, another pass lowers all dynamic variables.

* We then iterate on all types and lower them. All type declarations in the
  language spec are lowered in sequence (arbitrary order), except base classes,
  which are lowered before classes that they derive from. This pass creates the
  actual ``CompiledType`` instances as well as the ``AbstractNodeData`` ones.

* Finally, the arguments' default values and the bodies of all properties are
  lowered.
"""

from __future__ import annotations

import abc
from collections import OrderedDict
from dataclasses import dataclass
import itertools
import os.path
from typing import (
    Any, Callable, ClassVar, Dict, List, Optional, Set, Tuple, Type, TypeVar,
    Union, cast, overload
)

import liblktlang as L

from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNodeType, AbstractNodeData, Argument, CompiledType, CompiledTypeRepo,
    EnumNodeAlternative, EnumType, Field, MetadataField, StructType, T,
    TypeRepo, UserField, resolve_type
)
from langkit.diagnostics import (
    Location, check_source_language, diagnostic_context, error,
    errors_checkpoint, non_blocking_error
)
import langkit.expressions as E
from langkit.expressions import (
    AbstractExpression, AbstractKind, AbstractProperty, AbstractVariable, Cast,
    Let, LocalVars, Property, PropertyDef, create_lazy_field
)
import langkit.expressions.logic as ELogic
from langkit.lexer import (
    Action, Alt, Case, Ignore, Lexer, LexerToken, Literal, Matcher, NoCaseLit,
    Pattern, RuleAssoc, TokenAction, TokenFamily, WithSymbol, WithText,
    WithTrivia
)
import langkit.names as names
from langkit.parsers import (
    Cut, Defer, Discard, DontSkip, Grammar, List as PList, Null, Opt,
    Or, Parser, Pick, Predicate, Skip, StopCut, _Row, _Token, _Transform
)


# List of annotations that we don't compute here but that we can safely ignore
ANNOTATIONS_WHITELIST = ['builtin']


def same_node(left: L.LktNode, right: L.LktNode) -> bool:
    """
    Return whether ``left`` and ``right`` designate the same node, regardless
    of generic instantiation information.

    .. todo:: This should probably belong to Liblktlang, in one form or
       another. See use cases.
    """
    return left.unit == right.unit and left.sloc_range == right.sloc_range


def parse_static_bool(ctx: CompileCtx, expr: L.Expr) -> bool:
    """
    Return the bool value that this expression denotes.
    """
    with ctx.lkt_context(expr):
        check_source_language(isinstance(expr, L.RefId)
                              and expr.text in ('false', 'true'),
                              'Boolean literal expected')

    return expr.text == 'true'


def extract_var_name(ctx: CompileCtx, id: L.Id) -> tuple[str, names.Name]:
    """
    Turn the lower cased name ``n`` into a valid Ada identifier (for code
    generation).
    """
    source_name = id.text
    var_name = (
        names.Name("Ignored")
        if source_name == "_" else
        names.Name("Local") + name_from_lower(ctx, "variable", id)
    )
    return source_name, var_name


def name_from_lower(ctx: CompileCtx, kind: str, id: L.Id) -> names.Name:
    """
    Validate "id" as a lower-case name and return the corresponding ``Name``
    instance.
    """
    with ctx.lkt_context(id):
        try:
            names.check_common(id.text)
        except ValueError as exc:
            error(str(exc))
        try:
            names.check_lower(id.text)
        except ValueError:
            error(f"lower case expected for {kind} names")
        return names.Name.from_lower(id.text)


def name_from_camel(ctx: CompileCtx, kind: str, id: L.Id) -> names.Name:
    """
    Validate "id" as a camel-case name and return the corresponding ``Name``
    instance.
    """
    with ctx.lkt_context(id):
        try:
            names.check_common(id.text)
        except ValueError as exc:
            error(str(exc))
        try:
            names.check_camel(id.text)
        except ValueError as exc:
            error(f"camel case expected for {kind} names ({exc})")
        return names.Name.from_camel(id.text)


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

    # Forward potential lexing/parsing errors to our diagnostics system
    for u, d in diagnostics:
        with diagnostic_context(Location.from_sloc_range(u, d.sloc_range)):
            non_blocking_error(d.message)
    errors_checkpoint()
    return list(units_map.values())


class Scope:
    """
    Lexical environment data structure, use to resolve named references.
    """

    @dataclass
    class Entity(metaclass=abc.ABCMeta):
        """
        Object that is registered in a scope.
        """
        name: str

        @abc.abstractproperty
        def diagnostic_name(self) -> str:
            """
            Name for this entity to use when creating diagnostics.
            """
            ...

    @dataclass
    class BuiltinEntity(Entity):
        """
        Any entity that is created automatically by Lkt.
        """
        pass

    @dataclass
    class BuiltinFunction(BuiltinEntity):
        """
        Builtin function, used to expose a DSL operation.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin function {self.name}"

    @dataclass
    class BuiltinType(BuiltinEntity):
        """
        Type created automatically by Lkt.
        """

        defer: TypeRepo.Defer
        """
        Reference to the corresponding compiled type.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin type {self.name}"

    @dataclass
    class BuiltinValue(BuiltinEntity):
        """
        Named value created automatically by Lkt.
        """

        value: AbstractExpression
        """
        Value to use for this during expression lowering.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the builtin value {self.value}"

    @dataclass
    class Generic(BuiltinEntity):
        """
        Generic declaration, always created automatically by Lkt.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the generic {self.name}"

    @dataclass
    class Trait(BuiltinEntity):
        """
        Trait declaration, always created automatically by Lkt.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the trait {self.name}"

    @dataclass
    class Exception(BuiltinEntity):
        """
        Exception type, always created automatically by Lkt.
        """

        constructor: Callable[[TypeRepo.Defer, str], E.BaseRaiseException]

        @property
        def diagnostic_name(self) -> str:
            return f"the exception {self.name}"

    @dataclass
    class UserEntity(Entity):
        """
        Entity defined in user code.
        """

        decl: L.Decl
        """
        Corresponding Lkt declaration node.
        """

        kind_name: ClassVar[str]
        """
        Name for the kind of this entity, to use when formatting diagnostics.
        """

        @property
        def diagnostic_name(self) -> str:
            loc = Location.from_lkt_node(self.decl)
            return (
                f"the {self.kind_name} {self.name} at {loc.gnu_style_repr()}"
            )

    @dataclass
    class Lexer(UserEntity):
        """
        Lexer declaration.
        """

        kind_name = "lexer"

    @dataclass
    class Grammar(UserEntity):
        """
        Grammar declaration.
        """

        kind_name = "grammar"

    @dataclass
    class UserType(UserEntity):
        """
        Type declaration.
        """

        defer: TypeRepo.Defer
        """
        Reference to the corresponding compiled type.
        """

        kind_name = "type"

    @dataclass
    class UserValue(UserEntity):
        """
        Value declaration.
        """

        variable: AbstractVariable
        """
        Value to use for this during expression lowering.
        """

    @dataclass
    class LocalVariable(UserValue):
        """
        Local variable declaration.
        """

        kind_name = "local variable"

    @dataclass
    class Argument(UserValue):
        """
        Function argument declaration.
        """

        kind_name = "argument"

    @dataclass
    class DynVar(UserEntity):
        """
        Dynamic variable declaration.

        Note that this is not a derivation of ``UserValue`` since dynamic
        variables cannot be used as-is: they first need to be bound either
        explicitly through a ``VarBind`` expression or with the
        ``@with_dynvars`` property annotation.
        """

        variable: E.DynamicVariable

        kind_name = "dynamic variable"

    def __init__(
        self,
        label: str,
        context: CompileCtx,
        parent: Scope | None = None,
    ):
        """
        :param label: Label for this scope to use when formatting diagnostics.
        :param context: Current compilation context.
        :param parent: Optional parent scope. When looking for an entity by
            name, the search escalates to the parent if we cannot find the
            entity in this scope.
        """
        self.label = label
        self.context = context
        self.parent = parent
        self.mapping: dict[str, Scope.Entity] = {}

    def add(self, entity: Scope.UserEntity) -> None:
        """
        Add a declaration to the current scope.

        Stop with a user-level error if there is already a declaration with the
        same name in this scope.
        """
        other_entity = self.mapping.get(entity.name)
        if other_entity is None:
            self.mapping[entity.name] = entity
        else:
            with self.context.lkt_context(entity.decl):
                other_label = (
                    str(other_entity.decl)
                    if isinstance(other_entity, Scope.UserEntity) else
                    "a builtin"
                )
                error(f"this declaration conflicts with {other_label}")

    def lookup(self, name: str) -> Scope.Entity:
        """
        Look for the declaration for a given name in this scope or one of its
        parents. Raise a ``KeyError`` exception if there is no such
        declaration.
        """
        scope: Scope | None = self
        while scope is not None:
            try:
                return scope.mapping[name]
            except KeyError:
                scope = scope.parent

        raise KeyError(f"no entity called '{name}' in {self.label}")

    def resolve(self, name: L.Expr) -> Scope.Entity:
        """
        Resolve the entity designated by ``name`` in this scope.

        Unlike ``lookup``, this create a diagnostic if the entity is not found.
        """
        if isinstance(name, L.RefId):
            try:
                return self.lookup(name.text)
            except KeyError as exc:
                with self.context.lkt_context(name):
                    error(exc.args[0])
        else:
            with self.context.lkt_context(name):
                error("invalid entity reference")

    def create_child(self, label: str) -> Scope:
        """
        Return a new scope whose ``self`` is the parent.
        """
        return Scope(label, self.context, self)

    def dump(self) -> None:
        """
        Debug helper: dump this scope and its parent on the standard output.
        """
        s: Scope | None = self
        while s is not None:
            print(f"{s.label}:")
            for k, v in sorted(s.mapping.items()):
                print(f"  {k}: {v}")
            s = s.parent


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

    # Report errors on the entry point Lkt source file (no particular line)
    with diagnostic_context(Location(file=lkt_units[0].filename)):
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

    def interpret(
        self, ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        """
        Subclasses must override this in order to interpret an annotation.

        This method must validate and interpret ``args`` and ``kwargs``, and
        return a value suitable for annotations processing.

        :param args: Positional arguments for the annotation.
        :param kwargs: Keyword arguments for the annotation.
        :param scope: Scope to use when resolving entities mentionned in the
            annotation's arguments.
        """
        raise NotImplementedError

    def parse_single_annotation(
        self,
        ctx: CompileCtx,
        result: Dict[str, Any],
        annotation: L.DeclAnnotation,
        scope: Scope,
    ) -> None:
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
            value = self.interpret(ctx, [], {}, scope)
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
            value = self.interpret(ctx, args, kwargs, scope)

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

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        return True


class StringLiteralAnnotationSpec(AnnotationSpec):
    """
    Convenience subclass for annotations that take a string literal.
    """
    def __init__(self, name: str):
        super().__init__(
            name, unique=True, require_args=True, default_value=None
        )

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        if (
            len(args) != 1
            or kwargs
            or not isinstance(args[0], L.StringLit)
            or args[0].p_is_prefixed_string
        ):
            error("exactly one position argument expected: a string literal")
        return args[0].p_denoted_value


class WithDefaultAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_default annotations for enum types.
    """
    def __init__(self) -> None:
        super().__init__("with_default", unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        check_source_language(
            len(args) == 1 and not kwargs,
            "exactly one positional argument expected",
        )
        return args[0]


class WithDynvarsAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @with_dynvars annotations for properties.
    """
    def __init__(self) -> None:
        super().__init__("with_dynvars", unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        result: list[tuple[Scope.DynVar, L.Expr | None]] = []

        def add(
            entity: Scope.Entity,
            default_value: L.Expr | None = None,
        ) -> None:
            """
            Append a dynamic variable to ``result``. This also performs
            validity checks on the arguments.

            :param entity: Entity that is supposed to be a dynamic variable
                (this is checked).
            :param default_value: If this dynamic variable is optional, default
                value for it.
            """
            if not isinstance(entity, Scope.DynVar):
                error(
                    "dynamic variable expected, got {entity.diagnostic_name}"
                )
            if entity in result:
                error("dynamic variables can appear at most once")
            result.append((entity, default_value))

        # Positional arguments are supposed to be just dynamic variable names
        for arg in args:
            with ctx.lkt_context(arg):
                entity = scope.resolve(arg)
                add(entity)

        # Keyword arguments are supposed to associate a dynamic variable name
        # ("name" below) to a default value for the dynamic variable in the
        # current property ("default_value" below).
        for name, default_value in kwargs.items():
            try:
                entity = scope.lookup(name)
            except KeyError as exc:
                error(exc.args[0])
            add(entity, default_value)

        return result


class SpacingAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @spacing annotations for token families.
    """
    def __init__(self) -> None:
        super().__init__('unparse_spacing', unique=False, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
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


class TokenAnnotationSpec(AnnotationSpec):
    """
    Interpreter for @text/symbol/trivia annotations for tokens.
    """
    def __init__(self, name: str):
        super().__init__(name, unique=True, require_args=True)

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
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

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        assert not kwargs
        assert len(args) == 1
        requested_name = args[0]

        full_decl = find_toplevel_decl(
            ctx, ctx.lkt_units, L.LexerDecl, "lexer"
        )
        decl = full_decl.f_decl
        assert isinstance(decl, L.LexerDecl)
        with ctx.lkt_context(requested_name):
            check_source_language(
                decl.f_syn_name.text == requested_name.text,
                f"No '{requested_name.text}' lexer found",
            )
            return decl


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
    entry_point: bool
    annotations = [FlagAnnotationSpec('main_rule'),
                   FlagAnnotationSpec('entry_point')]


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
    synthetic: bool
    annotations = [
        FlagAnnotationSpec('has_abstract_list'),
        FlagAnnotationSpec('synthetic'),
    ]


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
    export: bool
    final: bool
    lazy: bool
    null_field: bool
    nullable: bool
    parse_field: bool
    trace: bool
    use_in_equality: bool
    annotations = [FlagAnnotationSpec('abstract'),
                   FlagAnnotationSpec('export'),
                   FlagAnnotationSpec('final'),
                   FlagAnnotationSpec('lazy'),
                   FlagAnnotationSpec('null_field'),
                   FlagAnnotationSpec('nullable'),
                   FlagAnnotationSpec('parse_field'),
                   FlagAnnotationSpec('trace'),
                   FlagAnnotationSpec('use_in_equality')]


@dataclass
class EnumAnnotations(ParsedAnnotations):
    with_default: L.Expr | None
    annotations = [
        WithDefaultAnnotationSpec()
    ]


@dataclass
class StructAnnotations(ParsedAnnotations):
    metadata: bool
    annotations = [FlagAnnotationSpec("metadata")]


@dataclass
class FunArgAnnotations(ParsedAnnotations):
    ignored: bool
    annotations = [FlagAnnotationSpec("ignored")]


@dataclass
class FunAnnotations(ParsedAnnotations):
    abstract: bool
    call_memoizable: bool
    call_non_memoizable_because: str | None
    export: bool
    external: bool
    final: bool
    ignore_unused: bool
    memoized: bool
    trace: bool
    uses_entity_info: bool
    uses_envs: bool
    with_dynvars: list[tuple[Scope.DynVar, L.Expr | None]] | None
    annotations = [
        FlagAnnotationSpec('abstract'),
        FlagAnnotationSpec('call_memoizable'),
        StringLiteralAnnotationSpec('call_non_memoizable_because'),
        FlagAnnotationSpec('export'),
        FlagAnnotationSpec('external'),
        FlagAnnotationSpec('final'),
        FlagAnnotationSpec('ignore_unused'),
        FlagAnnotationSpec('memoized'),
        FlagAnnotationSpec('trace'),
        FlagAnnotationSpec('uses_entity_info'),
        FlagAnnotationSpec('uses_envs'),
        WithDynvarsAnnotationSpec(),
    ]


def check_no_annotations(full_decl: L.FullDecl) -> None:
    """
    Check that the declaration has no annotation.
    """
    check_source_language(
        len(full_decl.f_decl_annotations) == 0, 'No annotation allowed'
    )


AnyPA = TypeVar('AnyPA', bound=ParsedAnnotations)


def parse_annotations(
    ctx: CompileCtx,
    annotation_class: Type[AnyPA],
    full_decl: L.FullDecl | L.DeclAnnotationList,
    scope: Scope,
) -> AnyPA:
    """
    Parse annotations according to the specs in
    ``annotation_class.annotations``. Return a ParsedAnnotations that contains
    the interpreted annotation values for each present annotation.

    :param annotation_class: ParsedAnnotations subclass for the result, holding
        the annotation specs to guide parsing.
    :param full_decl: Declaration whose annotations are to be parsed, or the
        annotations themselves.
    :param scope: Scope to use when resolving entities mentionned in the
        annotation's arguments.
    """
    # Build a mapping for all specs
    specs_map: Dict[str, AnnotationSpec] = {}
    for s in annotation_class.annotations:
        assert s.name not in specs_map
        specs_map[s.name] = s

    # Process annotations
    annotations = (
        full_decl
        if isinstance(full_decl, L.DeclAnnotationList) else
        full_decl.f_decl_annotations
    )
    values: Dict[str, Any] = {}
    for a in annotations:
        name = a.f_name.text
        spec = specs_map.get(name)
        with ctx.lkt_context(a):
            if spec is None:
                if name not in ANNOTATIONS_WHITELIST:
                    check_source_language(
                        False, 'Invalid annotation: {}'.format(name)
                    )
            else:
                spec.parse_single_annotation(ctx, values, a, scope)

    # Use the default value for absent annotations
    for s in annotation_class.annotations:
        values.setdefault(s.name, s.default_value)

    # Create the namespace object to hold results
    return annotation_class(**values)  # type: ignore


@dataclass
class FunctionSignature:
    """
    Specification of required/accepted arguments for a function.
    """

    positional_args: int
    """
    Number of positional-only arguments that are required to call such
    functions.
    """

    keyword_args: set[str]
    """
    Accepted keyword arguments, all optional.
    """

    accepts_others: bool = False
    """
    Whether more positional arguments than ``positional_args`` are accepted.
    """

    def match(self, ctx: CompileCtx, call: L.CallExpr) -> ParsedArgs:
        """
        Match call arguments against this signature. If successful, return the
        parsed arguments. Abort with a user error otherwise.
        """
        result = ParsedArgs([], {}, [])
        for arg in call.f_args:
            if arg.f_name:
                # This is a keyword argument
                name = arg.f_name.text
                with ctx.lkt_context(arg.f_name):
                    check_source_language(
                        name not in result.keyword_args,
                        "keyword arguments can be passed at most once",
                    )
                    check_source_language(
                        name in self.keyword_args,
                        "unexpected keyword argument",
                    )
                result.keyword_args[name] = arg.f_value

            else:
                # This is a positional argument
                result.positional_args.append(arg.f_value)

        # Make sure we got the expected number of positional arguments.  If we
        # have more than expected and if this function can accept them, move
        # the extra arguments to "result.others".
        expected = self.positional_args
        actual = len(result.positional_args)
        if self.accepts_others and actual > expected:
            result.other_args = result.positional_args[expected:]
            result.positional_args = result.positional_args[:expected]

        elif actual != expected:
            loc_node = (
                call.f_name.f_suffix
                if isinstance(call.f_name, L.DotExpr) else
                call
            )
            with ctx.lkt_context(loc_node):
                error(
                    f"{expected} positional argument(s) expected, got {actual}"
                )
        return result


@dataclass
class ParsedArgs:
    """
    Arguments parsed from a call and a corresponding function signature.
    """
    positional_args: list[L.Expr]
    keyword_args: dict[str, L.Expr]
    other_args: list[L.Expr]


collection_iter_signature = FunctionSignature(1, set())
"""
Signature for ".all"/".any".
"""

do_signature = FunctionSignature(1, {"default_val"})
"""
Signature for ".do".
"""

domain_signature = FunctionSignature(2, set())
"""
Signature for "%domain".
"""

dynamic_lexical_env_signature = FunctionSignature(
    1, {"assoc_resolver", "transitive_parent"}
)
"""
Signature for the "dynamic_lexical_env" builtin function.
"""

empty_signature = FunctionSignature(0, set())
"""
Signature for a function that takes no argument.
"""

eq_signature = FunctionSignature(2, {"conv_prop"})
"""
Signature for "%eq".
"""

filtermap_signature = FunctionSignature(2, set())
"""
Signature for ".filtermap".
"""

logic_all_any_signature = FunctionSignature(1, set())
"""
Signature for "%all" and for "%any".
"""

predicate_signature = FunctionSignature(2, set(), accepts_others=True)
"""
Signature for "%predicate".
"""

propagate_signature = FunctionSignature(2, set(), accepts_others=True)
"""
Signature for "%propagate".
"""


def create_lexer(ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]) -> Lexer:
    """
    Create and populate a lexer from a Lktlang unit.

    :param lkt_units: Non-empty list of analysis units where to look for the
        grammar.
    """
    # TODO: once the DSL is no more, use the same root scope as in the Lkt
    # types lowering pass.
    root_scope = Scope("the root scope", ctx)

    # Look for the LexerDecl node in top-level lists
    full_lexer = find_toplevel_decl(ctx, lkt_units, L.LexerDecl, 'lexer')
    assert isinstance(full_lexer.f_decl, L.LexerDecl)

    # Ensure the lexer name has proper casing
    _ = name_from_lower(ctx, "lexer", full_lexer.f_decl.f_syn_name)

    with ctx.lkt_context(full_lexer):
        lexer_annot = parse_annotations(
            ctx, LexerAnnotations, full_lexer, root_scope
        )

    patterns: Dict[names.Name, Tuple[str, Location]] = {}
    """
    Mapping from pattern names to the corresponding regular expression.
    """

    token_family_sets: Dict[names.Name, Tuple[Set[TokenAction], Location]] = {}
    """
    Mapping from token family names to the corresponding sets of tokens that
    belong to this family, and the location for the token family declaration.
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
            name = name_from_lower(ctx, "token family", f.f_syn_name)
            token_set, _ = token_family_sets.setdefault(
                name,
                (set(), Location.from_lkt_node(f)),
            )

            for r in f.f_rules:
                if not isinstance(r.f_decl, L.GrammarRuleDecl):
                    error('Only lexer rules allowed in family blocks')
                process_token_rule(r, token_set)

            family_annotations = parse_annotations(
                ctx,
                TokenFamilyAnnotations,
                cast(L.FullDecl, f.parent),
                root_scope,
            )

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
                ctx, TokenAnnotations, r, root_scope
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
            token_name = (
                None
                if token_lower_name == "_"
                else name_from_lower(ctx, "token", r.f_decl.f_syn_name)
            )

            check_source_language(
                token_lower_name not in ('termination', 'lexing_failure'),
                '{} is a reserved token name'.format(token_lower_name)
            )
            check_source_language(token_name not in tokens,
                                  'Duplicate token name')

            token = token_cons(start_ignore_layout, end_ignore_layout)
            if token_name is not None:
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
        name = name_from_lower(ctx, "pattern", decl.f_syn_name)

        with ctx.lkt_context(decl):
            check_source_language(name not in patterns,
                                  'Duplicate pattern name')
            with ctx.lkt_context(decl.f_decl_type):
                check_source_language(
                    decl.f_decl_type is None,
                    "Types are not allowed in lexer declarations"
                )
            if (
                not isinstance(decl.f_val, L.StringLit)
                or not decl.f_val.p_is_regexp_literal
            ):
                error('Pattern string literal expected')
            patterns[name] = (
                decl.f_val.p_denoted_value, Location.from_lkt_node(decl)
            )

    def lower_matcher(expr: L.GrammarExpr) -> Matcher:
        """
        Lower a token matcher to our internals.
        """
        loc = Location.from_lkt_node(expr)
        with ctx.lkt_context(expr):
            if isinstance(expr, L.TokenLit):
                return Literal(expr.p_denoted_value, location=loc)
            elif isinstance(expr, L.TokenNoCaseLit):
                return NoCaseLit(expr.f_lit.p_denoted_value, location=loc)
            elif isinstance(expr, L.TokenPatternLit):
                return Pattern(expr.p_denoted_value, location=loc)
            else:
                error('Invalid lexing expression')

    def lower_token_ref(ref: L.RefId) -> Action:
        """
        Return the Token that `ref` refers to.
        """
        with ctx.lkt_context(ref):
            token_name = names.Name.check_from_lower(ref.text)
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
    for name, (token_set, loc) in token_family_sets.items():
        tf = TokenFamily(*list(token_set), location=loc)
        token_families[name] = tf
        items[name.camel] = tf
    token_class = type('Token', (LexerToken, ), items)

    # Create the Lexer instance and register all patterns and lexing rules
    result = Lexer(token_class,
                   lexer_annot.track_indent,
                   pre_rules)
    for name, (regexp, loc) in patterns.items():
        result._add_pattern(name.lower, regexp, location=loc)
    result.add_rules(*rules)

    # Register spacing/newline rules
    for f1_name, f2_ref in spacings:
        f2_name = names.Name.check_from_lower(f2_ref.text)
        with ctx.lkt_context(f2_ref):
            check_source_language(
                f2_name in token_families,
                'Unknown token family: {}'.format(f2_name.lower)
            )
        result.add_spacing((token_families[f1_name],
                            token_families[f2_name]))
    result.add_newline_after(*newline_after)

    return result


def extract_doc(doc: L.Doc) -> str:
    """
    Turn a Doc node into the corresponding doc string.
    """
    # Extract the text from all doc lines
    lines: List[str] = []
    for d_line in doc.f_lines:
        line = d_line.text
        assert line.startswith("##")
        lines.append(line[2:])

    # Remove the biggest common indentation
    if lines:
        common_indent = min(
            len(line) - len(line.lstrip(" "))
            for line in lines
        )
        lines = [line[common_indent:] for line in lines]

    return "\n".join(lines)


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
    # TODO: once the DSL is no more, use the same root scope as in the Lkt
    # types lowering pass.
    root_scope = Scope("the root scope", ctx)

    # Look for the GrammarDecl node in top-level lists
    full_grammar = find_toplevel_decl(ctx, lkt_units, L.GrammarDecl, 'grammar')
    assert isinstance(full_grammar.f_decl, L.GrammarDecl)

    # Ensure the grammar name has proper casing
    _ = name_from_lower(ctx, "grammar", full_grammar.f_decl.f_syn_name)

    with ctx.lkt_context(full_grammar):
        parse_annotations(ctx, GrammarAnnotations, full_grammar, root_scope)

    # Collect the list of grammar rules. This is where we check that we only
    # have grammar rules, that their names are unique, and that they have valid
    # annotations.
    all_rules = OrderedDict()
    main_rule_name = None
    entry_points: Set[str] = set()
    for full_rule in full_grammar.f_decl.f_rules:
        with ctx.lkt_context(full_rule):
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

            all_rules[rule_name] = (full_rule.f_doc, r.f_expr)

    # Now create the result grammar
    if main_rule_name is None:
        with ctx.lkt_context(full_grammar.f_decl):
            error("main rule missing (@main_rule annotation)")
    result = Grammar(
        main_rule_name, entry_points, Location.from_lkt_node(full_grammar)
    )

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
            with ctx.lkt_context(node_ref.f_type_name):
                check_source_language(
                    node_ref.f_type_name.text == u'ASTList',
                    'Bad generic type name: only ASTList is valid in this'
                    ' context'
                )

            params = node_ref.f_params
            with ctx.lkt_context(node_ref):
                check_source_language(
                    len(params) == 2,
                    '2 type arguments expected, got {}'.format(len(params))
                )
            node_params = [resolve_node_ref(cast(NodeRefTypes, p))
                           for p in params]
            assert node_params[0] == T.root_node
            return node_params[1].list

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

    def lower_or_none(
        rule: Union[None, L.GrammarExpr, L.GrammarExprList]
    ) -> Optional[Parser]:
        """
        Like ``lower``, but also accept null grammar expressions.
        """
        return None if rule is None else lower(rule)

    def lower(
        rule: Union[L.GrammarExpr, L.GrammarExprList]
    ) -> Parser:
        """
        Helper to lower one parser.

        :param rule: Grammar rule to lower.
        """
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
                    with ctx.lkt_context(rule.f_token_name):
                        error(f"Unknown token: {token_name}")

                match_text = ''
                if rule.f_expr:
                    # The grammar is supposed to mainain this invariant
                    assert isinstance(rule.f_expr, L.TokenLit)
                    match_text = rule.f_expr.p_denoted_value

                return _Token(val=val, match_text=match_text, location=loc)

            elif isinstance(rule, L.TokenLit):
                return _Token(rule.p_denoted_value, location=loc)

            elif isinstance(rule, L.GrammarList):
                return PList(
                    lower(rule.f_expr),
                    empty_valid=rule.f_kind.text == '*',
                    list_cls=resolve_node_ref_or_none(rule.f_list_type),
                    sep=lower_or_none(rule.f_sep),
                    location=loc
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

    for name, (rule_doc, rule_expr) in grammar._all_lkt_rules.items():
        grammar._add_rule(name, lower(rule_expr), extract_doc(rule_doc))


# Mapping to associate declarations to the corresponding AbstractVariable
# instances. This is useful when lowering expressions.
LocalsEnv = Dict[L.BaseValDecl, AbstractVariable]


class LktTypesLoader:
    """
    Helper class to instantiate ``CompiledType`` for all types described in
    Lkt.
    """

    @dataclass
    class Generics:
        ast_list: Scope.Generic
        analysis_unit: Scope.Generic
        array: Scope.Generic
        entity: Scope.Generic
        iterator: Scope.Generic
        lexical_env: Scope.Generic
        node: Scope.Generic

    @dataclass
    class Functions:
        dynamic_lexical_env: Scope.BuiltinFunction

    # Map Lkt type declarations to the corresponding CompiledType instances, or
    # to None when the type declaration is currently being lowered. Keeping a
    # None entry in this case helps detecting illegal circular type
    # dependencies.
    compiled_types: Dict[L.TypeDecl, Optional[CompiledType]]

    @dataclass
    class PropertyToLower:
        prop: PropertyDef
        """
        The property whose expression must be lowered.
        """

        arguments: list[L.FunArgDecl]
        """
        Arguments for this property.
        """

        dynamic_vars: list[tuple[E.DynamicVariable, L.Expr | None]] | None
        """
        Dynamic variables for this property, and optional default value for
        each one. If None, inherit dynamic variables from the base property.
        """

    @dataclass
    class PropertyAndExprToLower(PropertyToLower):
        body: L.Expr
        """
        Root expression to lower.
        """

        scope: Scope
        """
        Scope to use during lowering. The property arguments must be available
        in it.
        """

    def __init__(self, ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]):
        """
        :param ctx: Context in which to create these types.
        :param lkt_units: Non-empty list of analysis units where to look for
            type declarations.
        """
        self.ctx = ctx
        self.root_scope = root_scope = Scope("the root scope", ctx)

        self.compiled_types: dict[L.Decl, CompiledType | None] = {}

        def builtin_type(
            name: str,
            internal_name: str | None = None,
        ) -> Scope.BuiltinType:
            """
            Create a builtin type for scopes.

            :param name: Name for this type in scopes.
            :param internal_name: If provided, name for the underlying compiled
                type. Use "name" if not provided.
            """
            return Scope.BuiltinType(
                name,
                T.deferred_type(internal_name or name),
            )

        self.generics = self.Generics(
            Scope.Generic("ASTList"),
            Scope.Generic("AnalysisUnit"),
            Scope.Generic("Array"),
            Scope.Generic("Entity"),
            Scope.Generic("Iterator"),
            Scope.Generic("LexicalEnv"),
            Scope.Generic("Node"),
        )
        self.node_builtin = Scope.BuiltinValue("node", E.Self)
        self.self_builtin = Scope.BuiltinValue("self", E.Entity)

        self.precondition_failure = Scope.Exception(
            "PreconditionFailure", E.PreconditionFailure
        )
        self.property_error = Scope.Exception("PropertyError", E.PropertyError)

        self.builtin_functions = self.Functions(
            Scope.BuiltinFunction("dynamic_lexical_env")
        )

        # Register builtins in the root scope
        with AbstractExpression.with_location(Location.builtin):
            for builtin in [
                builtin_type("AnalysisUnitKind"),
                builtin_type("BigInt"),
                builtin_type("Bool"),
                builtin_type("Char", "Character"),
                builtin_type("DesignatedEnv"),
                builtin_type("DesignatedEnvKind"),
                builtin_type("EntityInfo"),
                builtin_type("EnvAssoc"),
                builtin_type("EnvRebindings"),
                builtin_type("Equation"),
                builtin_type("InnerEnvAssoc"),
                builtin_type("Int"),
                builtin_type("LogicVar"),
                builtin_type("LookupKind"),
                builtin_type("RefCategories"),
                builtin_type("String"),
                builtin_type("Symbol"),
                builtin_type("Token"),
                Scope.BuiltinValue("false", E.Literal(False)),
                Scope.BuiltinValue("true", E.Literal(True)),
                self.node_builtin,
                self.self_builtin,
                self.precondition_failure,
                self.property_error,
                self.generics.ast_list,
                self.generics.analysis_unit,
                self.generics.array,
                self.generics.entity,
                self.generics.iterator,
                self.generics.lexical_env,
                self.generics.node,
                Scope.Trait("ErrorNode"),
                Scope.Trait("TokenNode"),
                self.builtin_functions.dynamic_lexical_env,
            ]:
                root_scope.mapping[builtin.name] = builtin

        # Go through all units and register all top-level definitions in the
        # root scope. This first pass allows to check for name uniqueness,
        # create TypeRepo.Defer objects and build the list of types to lower.
        type_decls: list[L.TypeDecl] = []
        dyn_vars: list[L.DynVarDecl] = []
        for unit in lkt_units:
            assert isinstance(unit.root, L.LangkitRoot)
            for full_decl in unit.root.f_decls:
                decl = full_decl.f_decl
                name = decl.f_syn_name.text
                if isinstance(decl, L.LexerDecl):
                    root_scope.add(Scope.Lexer(name, decl))
                elif isinstance(decl, L.GrammarDecl):
                    root_scope.add(Scope.Grammar(name, decl))
                elif isinstance(decl, L.TypeDecl):
                    root_scope.add(
                        Scope.UserType(name, decl, T.deferred_type(name))
                    )
                    type_decls.append(decl)
                elif isinstance(decl, L.DynVarDecl):
                    dyn_vars.append(decl)
                else:
                    error(
                        "invalid top-level declaration:"
                        f" {decl.p_decl_type_name}"
                    )

        # Make sure we have a Metadata type registered in the root scope so
        # that Lkt code can refer to it.
        if "Metadata" not in root_scope.mapping:
            self.root_scope.mapping["Metadata"] = Scope.BuiltinType(
                name="Metadata",
                defer=T.deferred_type("Metadata"),
            )

        # Create dynamic variables
        for dyn_var_decl in dyn_vars:
            name_node = dyn_var_decl.f_syn_name

            # Ensure the dynamic variable name has proper casing
            _ = name_from_lower(self.ctx, "dynamic variable", name_node)

            name = name_node.text
            dyn_var = E.DynamicVariable(
                name=name,
                type=self.resolve_type(dyn_var_decl.f_decl_type, root_scope),
                doc=self.ctx.lkt_doc(dyn_var_decl),
            )
            root_scope.add(Scope.DynVar(name, dyn_var_decl, dyn_var))

        # Now create CompiledType instances for each user type. To properly
        # handle node derivation, this recurses on bases first and reject
        # inheritance loops.
        self.properties_to_lower: list[LktTypesLoader.PropertyToLower] = []
        for type_decl in type_decls:
            self.lower_type_decl(type_decl)

        # If user code does not define one, create a default Metadata struct
        if CompiledTypeRepo.env_metadata is None:
            self.ctx.create_default_metadata()

        for entity in self.root_scope.mapping.items():
            if isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                entity._type = resolve_type(entity.defer)
                # TODO: resolve all deferred types (member types and argument
                # types).

        # Now that all user-defined compiled types are known, we can start
        # lowering expressions. Start with default values for property
        # arguments and dynamic variables.
        for to_lower in self.properties_to_lower:
            for arg_decl, arg in zip(
                to_lower.arguments, to_lower.prop.arguments
            ):
                if arg_decl.f_default_val is not None:
                    value = self.lower_static_expr(arg_decl.f_default_val)
                    value.prepare()
                    arg.set_default_value(value)

            if to_lower.dynamic_vars is not None:
                to_lower.prop.set_dynamic_vars(
                    [
                        (
                            dynvar,
                            None
                            if init_expr is None else
                            self.lower_static_expr(init_expr)
                        )
                        for dynvar, init_expr in to_lower.dynamic_vars
                    ]
                )

        # Now that all types and properties ("declarations") are available,
        # lower the property expressions themselves.
        for to_lower in self.properties_to_lower:
            if isinstance(to_lower, self.PropertyAndExprToLower):
                with to_lower.prop.bind():
                    to_lower.prop.expr = self.lower_expr(
                        to_lower.body, to_lower.scope, to_lower.prop.vars
                    )

    def resolve_entity(self, name: L.Expr, scope: Scope) -> Scope.Entity:
        """
        Resolve the entity designated by ``name`` in the given scope.
        """
        return scope.resolve(name)

    def resolve_generic(self, name: L.Expr, scope: Scope) -> Scope.Generic:
        """
        Like ``resolve_entity``, but for generics specifically.
        """
        result = self.resolve_entity(name, scope)
        if isinstance(result, Scope.Generic):
            return result
        else:
            with self.ctx.lkt_context(name):
                error("generic expected, got {result.diagnostic_name}")

    def resolve_type(self, name: L.TypeRef, scope: Scope) -> TypeRepo.Defer:
        """
        Like ``resolve_entity``, but for types specifically.
        """
        if isinstance(name, L.GenericTypeRef):
            with self.ctx.lkt_context(name):
                generic = self.resolve_generic(name.f_type_name, scope)
                type_args = list(name.f_params)
                if generic == self.generics.ast_list:
                    if len(type_args) != 2:
                        error(
                            f"{generic.name} expects two type arguments: the"
                            " root node and the list element type"
                        )
                    root_node_ref, element_type_ref = type_args

                    # TODO: validate that root_node_ref is the root node and
                    # that element_type_ref is a node type. The only way to
                    # validate this currently is to perform lowering, so we
                    # have a chicken and egg problem: we may already be
                    # lowering the root node.
                    del root_node_ref

                    return self.resolve_type(element_type_ref, scope).list

                elif generic == self.generics.analysis_unit:
                    if len(type_args) != 1:
                        error(
                            f"{generic.name} expects one type argument: the"
                            " root node"
                        )
                    root_node_ref, = type_args

                    # TODO: validate that root_node_ref is the root node (see
                    # above).
                    del root_node_ref

                    return T.AnalysisUnit

                elif generic == self.generics.array:
                    if len(type_args) != 1:
                        error(
                            f"{generic.name} expects one type argument: the"
                            " element type"
                        )
                    element_type, = type_args
                    return self.resolve_type(element_type, scope).array

                elif generic == self.generics.entity:
                    if len(type_args) != 1:
                        error(
                            f"{generic.name} expects one type argument: the"
                            " node type"
                        )
                    node_type, = type_args

                    # TODO: validate that node_type is indeed a node type

                    return self.resolve_type(node_type, scope).entity

                elif generic == self.generics.iterator:
                    if len(type_args) != 1:
                        error(
                            f"{generic.name} expects one type argument: the"
                            " element type"
                        )
                    element_type, = type_args
                    return self.resolve_type(element_type, scope).iterator

                elif generic == self.generics.node:
                    error(
                        "this generic trait is supposed to be used only in"
                        " the 'implements' part of the root node type"
                        " declaration"
                    )

                else:
                    # User code cannot define new generics, so there cannot
                    # possibly be other generics.
                    assert False

        elif isinstance(name, L.SimpleTypeRef):
            return self.resolve_type_expr(name.f_type_name, scope)

        else:
            with self.ctx.lkt_context(name):
                error("invalid type reference")

    def resolve_type_expr(self, name: L.Expr, scope: Scope) -> TypeRepo.Defer:
        """
        Like ``resolve_type``, but working on a type expression directly.
        """
        with self.ctx.lkt_context(name):
            if isinstance(name, L.RefId):
                entity = self.resolve_entity(name, scope)
                if isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                    return entity.defer
                else:
                    error(f"type expected, got {entity.diagnostic_name}")

            elif isinstance(name, L.DotExpr):
                # This must be a reference to an enum node:
                # "EnumNode.Alternative".
                dot_expr = name
                prefix = self.resolve_type_expr(dot_expr.f_prefix, scope)
                suffix = dot_expr.f_suffix

                def getter() -> ASTNodeType:
                    enum_node = resolve_type(prefix)

                    if (
                        # Make sure that enum_node is an enum node...
                        not isinstance(enum_node, ASTNodeType)
                        or not enum_node.is_enum_node

                        # ... and not an enum node alternative
                        or enum_node.base is None
                        or enum_node.base.is_enum_node
                    ):
                        with self.ctx.lkt_context(dot_expr.f_prefix):
                            error("base enum node expected")

                    try:
                        return enum_node._alternatives_map[suffix.text]
                    except KeyError:
                        with self.ctx.lkt_context(suffix):
                            error("no such alternative")

                return TypeRepo.Defer(getter, f"{prefix.label}.{suffix}")
            else:
                error("invalid type reference")

    @overload
    def resolve_property(self, name: L.Expr) -> TypeRepo.Defer: ...

    @overload
    def resolve_property(self, name: None) -> None: ...

    def resolve_property(self, name: L.Expr | None) -> TypeRepo.Defer | None:
        """
        Like ``resolve_entity``, but for properties specifically.
        """
        if name is None:
            return None

        if not isinstance(name, L.DotExpr):
            error(
                "invalid reference to a property (should be:"
                " ``T.property_name``)"
            )

        prefix = self.resolve_type_expr(name.f_prefix, self.root_scope)
        suffix_node = name.f_suffix

        def getter() -> PropertyDef:
            node_type = resolve_type(prefix)
            member = (
                node_type
                .get_abstract_node_data_dict()
                .get(suffix_node.text, None)
            )
            if not isinstance(member, PropertyDef):
                with self.ctx.lkt_context(suffix_node):
                    error("property expected")
            return member

        return TypeRepo.Defer(getter, name.text)

    def resolve_and_lower_node(
        self,
        name: L.TypeRef,
        scope: Scope,
    ) -> ASTNodeType:
        """
        Resolve a type reference and lower it, checking that it is a node type.
        """
        with self.ctx.lkt_context(name):
            # There are only two legal cases: the base type is just a node
            # class defined in user code (SimpleTypeRef) or it is a bare node
            # list instantiation (GenericTypeRef). Reject everything else.
            if isinstance(name, L.SimpleTypeRef):
                # We have a direct node class reference: first fetch the Lkt
                # declaration for it.
                try:
                    entity = scope.lookup(name.text)
                except KeyError as exc:
                    error(exc.args[1])
                if not isinstance(entity, Scope.UserType):
                    error(f"node type expected, got {entity.diagnostic_name}")
                base_type_decl = entity.decl
                assert isinstance(base_type_decl, L.TypeDecl)

                # Then, force its lowering
                base_type = self.lower_type_decl(base_type_decl)
                if not isinstance(base_type, ASTNodeType):
                    error("node type expected")
                return base_type

            elif isinstance(name, L.GenericTypeRef):
                # This must be a node list instantiation: validate the
                # instantiation itself.
                generic = self.resolve_generic(name.f_type_name, scope)
                if generic != self.generics.ast_list:
                    error(
                        "the only generic allowed in this context is"
                        f" {self.generics.ast_list.name}"
                    )

                # Lower the root and element nodes
                type_args = [
                    self.resolve_and_lower_node(t, scope)
                    for t in name.f_params
                ]
                check_source_language(
                    len(type_args) == 2,
                    "{generic.name} expects two type arguments: the root node"
                    " and the list element type"
                )
                root_node, element_type = type_args

                check_source_language(
                    root_node.is_root_node,
                    f"in {generic.name}[N1, N2], N1 is supposed to be the root"
                    " node"
                )
                return element_type.list

            else:
                error("invalid node type reference")

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
                # The type is not lowered yet: let's do it. Add the sentinel to
                # reject type inheritance loop during recursion.
                self.compiled_types[decl] = None
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
            full_decl = decl.parent
            assert isinstance(full_decl, L.FullDecl)
            if isinstance(decl, L.BasicClassDecl):

                specs = (EnumNodeAnnotations
                         if isinstance(decl, L.EnumClassDecl)
                         else NodeAnnotations)
                result = self.create_node(
                    decl,
                    parse_annotations(
                        self.ctx, specs, full_decl, self.root_scope
                    ),
                )

            elif isinstance(decl, L.EnumTypeDecl):
                result = self.create_enum(
                    decl,
                    parse_annotations(
                        self.ctx, EnumAnnotations, full_decl, self.root_scope
                    )
                )

            elif isinstance(decl, L.StructDecl):
                result = self.create_struct(
                    decl,
                    parse_annotations(
                        self.ctx, StructAnnotations, full_decl, self.root_scope
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
        allowed_field_types: Tuple[Type[AbstractNodeData], ...],
        user_field_public: bool,
        struct_name: str,
    ) -> AbstractNodeData:
        """
        Lower the field described in ``decl``.

        :param allowed_field_types: Set of types allowed for the fields to
            load.
        :param user_field_public: Whether user fields should be made public.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FieldDecl)

        # Ensure the dynamic variable name has proper casing
        _ = name_from_lower(self.ctx, "type member", decl.f_syn_name)

        annotations = parse_annotations(
            self.ctx, FieldAnnotations, full_decl, self.root_scope
        )
        field_type = self.resolve_type(decl.f_decl_type, self.root_scope)
        doc = self.ctx.lkt_doc(decl)

        cls: Type[AbstractNodeData]
        constructor: Callable[..., AbstractNodeData]
        kwargs: Dict[str, Any] = {'type': field_type, 'doc': doc}

        check_source_language(
            annotations.parse_field or not annotations.null_field,
            '@nullable is valid only for parse fields'
        )

        body: L.Expr | None = None
        if annotations.lazy:
            check_source_language(
                not annotations.null_field,
                'Lazy fields cannot be null'
            )
            check_source_language(
                not annotations.final,
                'Lazy fields are implicitly final'
            )
            cls = PropertyDef
            constructor = create_lazy_field

            body = decl.f_default_val

            kwargs = {
                'expr': None,
                'doc': doc,
                'public': annotations.export,
                'return_type': field_type,
                'kind': (AbstractKind.abstract
                         if annotations.abstract
                         else AbstractKind.concrete),
                'activate_tracing': annotations.trace,
            }

        elif annotations.parse_field:
            assert decl.f_default_val is None
            check_source_language(
                not annotations.export,
                'Parse fields are implicitly exported'
            )
            check_source_language(
                not annotations.final,
                'Concrete parse fields are implicitly final'
            )
            check_source_language(
                not annotations.lazy,
                'Parse fields cannot be lazy'
            )
            check_source_language(
                not annotations.trace,
                'Parse fields cannot be traced'
            )
            cls = constructor = Field
            kwargs['abstract'] = annotations.abstract
            kwargs['null'] = annotations.null_field
            kwargs['nullable'] = annotations.nullable

        else:
            check_source_language(
                not annotations.abstract,
                'Regular fields cannot be abstract'
            )
            check_source_language(
                not annotations.export,
                'Regular fields are implicitly exported'
            )
            check_source_language(
                not annotations.final,
                'Regular fields are implicitly final'
            )
            check_source_language(
                not annotations.lazy,
                'Regular fields cannot be lazy'
            )
            check_source_language(
                not annotations.null_field,
                'Regular fields cannot be null'
            )
            check_source_language(
                not annotations.trace,
                'Regular fields cannot be traced'
            )
            cls = constructor = UserField
            kwargs['public'] = user_field_public
            kwargs['default_value'] = (
                self.lower_expr(decl.f_default_val, self.root_scope, None)
                if decl.f_default_val
                else None
            )

            # If this field belongs to the metadata struct, use the appropriate
            # constructor. Reject @use_in_equality annotations otherwise, as
            # they are valid only for metadata fields.
            if allowed_field_types == (MetadataField, ):
                cls = constructor = MetadataField
                kwargs["use_in_equality"] = annotations.use_in_equality
            else:
                check_source_language(
                    not annotations.use_in_equality,
                    "Only metadata fields can have the @use_in_equality"
                    " annotation",
                )

        check_source_language(
            issubclass(cls, allowed_field_types),
            'Invalid field type in this context'
        )

        result = constructor(**kwargs)

        # If this field has an initialization expression implemented as
        # property, plan to lower it later.
        if isinstance(result, PropertyDef):
            assert body is not None
            arguments, scope = self.lower_property_arguments(
                prop=result,
                arg_decl_list=None,
                label=(
                    "initializer for lazy field"
                    f" {struct_name}.{decl.f_syn_name.text}"
                ),
            )
            self.properties_to_lower.append(
                self.PropertyAndExprToLower(
                    result, arguments, None, body, scope
                )
            )

        return result

    def lower_static_expr(self, expr: L.Expr) -> AbstractExpression:
        """
        Lower the given expression, checking that it is a valid compile time
        known value.
        """
        with self.ctx.lkt_context(expr):
            # Accept simple identifiers that refer to builtin values
            if isinstance(expr, L.RefId):
                entity = self.resolve_entity(expr, self.root_scope)
                if (
                    isinstance(entity, Scope.BuiltinValue)
                    and isinstance(entity.value, E.Literal)
                ):
                    return entity.value

            elif (
                # Also accept character and number literals, as well as null
                # expressions.
                isinstance(expr, (L.CharLit, L.NullLit, L.NumLit))
                or (
                    # Finally, also accept references to enum values
                    isinstance(expr, L.DotExpr)
                    and isinstance(expr.f_prefix, L.RefId)
                )
            ):
                return self.lower_expr(expr, self.root_scope, None)

            error("static expression expected in this context")

    def lower_expr(self,
                   expr: L.Expr,
                   env: Scope,
                   local_vars: Optional[LocalVars]) -> AbstractExpression:
        """
        Lower the given expression.

        :param expr: Expression to lower.
        :param env: Scope to use when resolving references.
        :param local_vars: If lowering a property expression, set of local
            variables for this property.
        """
        # Counter to generate unique names
        counter = itertools.count(0)

        def add_lambda_arg_to_scope(
            scope: Scope,
            arg: L.LambdaArgDecl,
            var: AbstractVariable
        ) -> None:
            """
            Helper to register a lambda expression argument in a scope.
            """
            scope.add(Scope.LocalVariable(arg.f_syn_name.text, arg, var))

        def var_for_lambda_arg(
            scope: Scope,
            arg: L.LambdaArgDecl,
            prefix: str,
            type: Optional[CompiledType] = None,
            create_local: bool = False,
        ) -> AbstractVariable:
            """
            Create an AbstractVariable to translate a lambda argument.

            This also registers this decl/variable association in ``env``.

            :param prefix: Lower-case prefix for the name of the variable in
                the generated code.
            """
            source_name, _ = extract_var_name(self.ctx, arg.f_syn_name)
            with AbstractExpression.with_location(Location.from_lkt_node(arg)):
                result = AbstractVariable(
                    names.Name.from_lower(f"{prefix}_{next(counter)}"),
                    source_name=source_name,
                    type=type,
                    create_local=create_local,
                )
            add_lambda_arg_to_scope(scope, arg, result)
            return result

        def extract_call_args(expr: L.CallExpr) -> Tuple[List[L.Expr],
                                                         Dict[str, L.Expr]]:
            """
            Extract positional and keyword arguments from a call expression.
            """
            args = []
            kwargs = {}
            for arg in expr.f_args:
                value = arg.f_value
                if arg.f_name:
                    kwargs[arg.f_name.text] = value
                else:
                    args.append(value)
            return args, kwargs

        @dataclass
        class LambdaInfo:
            """
            Information about the call to a builtin operation that takes a
            lambda as the first argument, plus optional keyword arguments.
            """
            kwargs: dict[str, L.Expr]
            """
            Keyword arguments passed after the lambda expression.
            """

            scope: Scope
            """
            New scope to lower lambda function arguments and inner expression.
            """

            largs: list[L.LambdaArgDecl | None]
            """
            List of arguments for this lambda expression, padded with None up
            to "max_lambda_args" list items.
            """

            expr: L.Expr
            """
            Lambda expression "body".
            """

        def extract_lambda(
            expr: L.LambdaExpr,
            min_lambda_args: int,
            max_lambda_args: int | None = None,
        ) -> tuple[Scope, list[L.LambdaArgDecl | None], L.Expr]:
            """
            Extract arguments/expr from a lambda expression.

            :param expr: Lambda expression to analyze.
            :param min_lambda_args: Minimum number of arguments expected for
                the lambda expression.
            :param max_lambda_args: Maximum number of arguments expected for
                the lambda expression. If left to None, expect exactly
                "min_lambda_args".
            """
            lambda_args: list[L.LambdaArgDecl | None] = (
                [None] * (max_lambda_args or min_lambda_args)
            )
            lambda_n_args = len(expr.f_params)
            with self.ctx.lkt_context(expr.f_params):
                if max_lambda_args is None:
                    check_source_language(
                        lambda_n_args == min_lambda_args,
                        f"exactly {min_lambda_args} arguments expected",
                    )
                else:
                    check_source_language(
                        lambda_n_args >= min_lambda_args,
                        f"at least {min_lambda_args} arguments expected",
                    )
                    check_source_language(
                        lambda_n_args <= max_lambda_args,
                        f"at most {max_lambda_args} arguments expected",
                    )
            for i, larg in enumerate(expr.f_params):
                lambda_args[i] = larg
                with self.ctx.lkt_context(larg):
                    # TODO: accepting type annotations and validating them
                    # could be useful for language spec readability.
                    check_source_language(
                        larg.f_decl_type is None,
                        "argument type must be implicit",
                    )
                    check_source_language(
                        larg.f_default_val is None,
                        "default values are not allowed here",
                    )

            loc = Location.from_lkt_node(expr)
            scope = env.create_child(
                f"scope for lambda expression at {loc.gnu_style_repr()}"
            )

            return (scope, lambda_args, expr.f_body)

        def extract_lambda_and_kwargs(
            expr: L.CallExpr,
            signature: FunctionSignature,
            min_lambda_args: int,
            max_lambda_args: int | None = None,
        ) -> LambdaInfo:
            """
            Extract arguments from a call expression, expecting the first
            positional argument to be a lambda expression.

            :param expr: Call expression that is supposed to pass the lambda
                expression.
            :param signature: Signature for the pseudo-function that is called.
            :param min_lambda_args: Minimum number of arguments expected for
                the lambda expression.
            :param max_lambda_args: Maximum number of arguments expected for
                the lambda expression. If left to None, expect exactly
                "min_lambda_args".
            """
            # Make sure the only positional argument is a lambda expression
            parsed_args = signature.match(self.ctx, expr)
            lambda_expr = parsed_args.positional_args[0]
            if not isinstance(lambda_expr, L.LambdaExpr):
                with self.ctx.lkt_context(lambda_expr):
                    error("lambda expression expected")

            # Extract info from the lambda expression itself
            scope, lambda_args, lambda_body = extract_lambda(
                lambda_expr, min_lambda_args, max_lambda_args
            )

            return LambdaInfo(
                parsed_args.keyword_args, scope, lambda_args, lambda_body
            )

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Wrapper around "_lower" to set the expression location.

            Calling this function instead of ``_lower`` below to lower
            individual expression nodes is what correctly assigns the Lkt
            location to each instantiated ``AbstractExpression``.
            """
            with AbstractExpression.with_location(
                Location.from_lkt_node(expr)
            ):
                return _lower(expr)

        def _lower(expr: L.Expr) -> AbstractExpression:
            """
            Do the actual expression lowering. Since all recursive calls use
            the same environment, this helper allows to skip passing it.
            """
            result: AbstractExpression

            if isinstance(expr, L.ArrayLiteral):
                elts = [lower(e) for e in expr.f_exprs]
                element_type = (
                    None
                    if expr.f_element_type is None else
                    self.resolve_type(expr.f_element_type, env)
                )
                return E.ArrayLiteral(elts, element_type=element_type)

            elif isinstance(expr, L.BigNumLit):
                text = expr.text
                assert text[-1] == 'b'
                return E.BigIntLiteral(int(text[:-1]))

            elif isinstance(expr, L.BinOp):
                # Lower both operands
                left = lower(expr.f_left)
                right = lower(expr.f_right)

                # Dispatch to the appropriate abstract expression constructor
                if isinstance(expr.f_op, L.OpEq):
                    return E.Eq(left, right)

                elif isinstance(expr.f_op, L.OpNe):
                    return E.Not(E.Eq(left, right))

                elif isinstance(expr.f_op, (L.OpLt, L.OpGt, L.OpLte, L.OpGte)):
                    operator = {
                        L.OpLt: E.OrderingTest.LT,
                        L.OpLte: E.OrderingTest.LE,
                        L.OpGt: E.OrderingTest.GT,
                        L.OpGte: E.OrderingTest.GE,
                    }[type(expr.f_op)]
                    return E.OrderingTest(operator, left, right)

                elif isinstance(expr.f_op, L.OpAnd):
                    return E.BinaryBooleanOperator(
                        E.BinaryBooleanOperator.AND, left, right
                    )

                elif isinstance(expr.f_op, L.OpOr):
                    return E.BinaryBooleanOperator(
                        E.BinaryBooleanOperator.OR, left, right
                    )

                else:
                    operator = {
                        L.OpAmp: '&',
                        L.OpPlus: '+',
                        L.OpMinus: '-',
                        L.OpMult: '*',
                        L.OpDiv: '/',
                    }[type(expr.f_op)]
                    return E.Arithmetic(left, right, operator)

            elif isinstance(expr, L.BlockExpr):
                assert local_vars is not None
                loc = Location.from_lkt_node(expr)
                sub_env = env.create_child(
                    f"scope for block at {loc.gnu_style_repr()}"
                )

                @dataclass
                class DeclAction:
                    var: AbstractVariable
                    init_expr: AbstractExpression
                    location: Location

                actions: list[DeclAction] = []

                for v in expr.f_val_defs:
                    source_name: str
                    source_var: L.Decl
                    var: AbstractVariable
                    init_abstract_expr: L.Expr

                    if isinstance(v, L.ValDecl):
                        # Create the AbstractVariable for this declaration
                        source_name = v.f_syn_name.text
                        source_var = v
                        source_name, v_name = extract_var_name(
                            self.ctx, v.f_syn_name
                        )
                        v_type = (
                            resolve_type(self.resolve_type(v.f_decl_type, env))
                            if v.f_decl_type else
                            None
                        )
                        with AbstractExpression.with_location(
                            Location.from_lkt_node(v)
                        ):
                            var = AbstractVariable(
                                v_name,
                                v_type,
                                create_local=True,
                                source_name=source_name,
                            )
                        init_abstract_expr = v.f_val

                    elif isinstance(v, L.VarBind):
                        source_name = v.f_name.text

                        # Look for the corresponding dynamic variable
                        entity = self.resolve_entity(v.f_name, sub_env)
                        if not isinstance(entity, Scope.DynVar):
                            error(
                                "dynamic variable expected, got"
                                f" {entity.diagnostic_name}"
                            )

                        source_var = entity.decl
                        var = entity.variable
                        init_abstract_expr = v.f_expr

                    else:
                        assert False, f'Unhandled def in BlockExpr: {v}'

                    # Make the declaration available to the inner expression
                    # lowering.
                    sub_env.add(
                        Scope.LocalVariable(source_name, source_var, var)
                    )

                    # Lower the declaration/bind initialization expression
                    init_expr = self.lower_expr(
                        init_abstract_expr, sub_env, local_vars
                    )

                    actions.append(
                        DeclAction(var, init_expr, Location.from_lkt_node(v))
                    )

                # Lower the block main expression and wrap it in declarative
                # blocks.
                result = self.lower_expr(expr.f_expr, sub_env, local_vars)
                for action in reversed(actions):
                    with AbstractExpression.with_location(action.location):
                        if isinstance(action.var, E.DynamicVariable):
                            result = getattr(var, "bind")(
                                action.init_expr, result
                            )
                        else:
                            result = Let(
                                ([action.var], [action.init_expr], result)
                            )
                return result

            elif isinstance(expr, L.CallExpr):
                call_expr = expr
                call_name = call_expr.f_name

                # TODO: get rid of this syntax? It is never legal for now...
                if isinstance(expr, L.NullCondCallExpr):
                    with self.ctx.lkt_context(expr):
                        error(
                            "'?' must be attached to the method prefix, not to"
                            " the call itself",
                        )

                def lower_args() -> Tuple[List[AbstractExpression],
                                          Dict[str, AbstractExpression]]:
                    """
                    Collect call positional and keyword arguments.
                    """
                    arg_nodes, kwarg_nodes = extract_call_args(call_expr)
                    args = [lower(v) for v in arg_nodes]
                    kwargs = {k: lower(v) for k, v in kwarg_nodes.items()}
                    return args, kwargs

                def lower_new(type_ref: TypeRepo.Defer) -> AbstractExpression:
                    """
                    Consider that this call creates a new struct, return the
                    corresponding New expression.
                    """
                    args, kwargs = lower_args()
                    check_source_language(
                        len(args) == 0,
                        "Positional arguments not allowed for struct"
                        " constructors",
                    )
                    return E.New(type_ref, **kwargs)

                # Depending on its name, a call can have different meanings...

                # If it is a simple identifier...
                if isinstance(call_name, L.RefId):
                    entity = self.resolve_entity(call_name, env)

                    # It can be a call to a built-in function
                    if entity == self.builtin_functions.dynamic_lexical_env:
                        parsed_args = dynamic_lexical_env_signature.match(
                            self.ctx, call_expr
                        )
                        trans_parent_expr = parsed_args.keyword_args.get(
                            "transitive_parent"
                        )
                        return E.DynamicLexicalEnv(
                            assocs_getter=lower(
                                parsed_args.positional_args[0]
                            ),
                            assoc_resolver=self.resolve_property(
                                parsed_args.keyword_args.get("assoc_resolver")
                            ),
                            transitive_parent=(
                                E.Literal(True)
                                if trans_parent_expr is None else
                                lower(trans_parent_expr)
                            ),
                        )

                    # It can be a New expression
                    elif isinstance(
                        entity, (Scope.BuiltinType, Scope.UserType)
                    ):
                        return lower_new(entity.defer)

                    # Everything else is illegal
                    with self.ctx.lkt_context(call_name):
                        error("invalid call prefix")

                # If the call name is a generic instantiation, it has to be a
                # reference to a struct type, and thus the call is a New
                # expression.
                elif isinstance(call_name, L.GenericInstantiation):
                    generic = self.resolve_generic(call_name.f_name, env)
                    type_args = call_name.f_args
                    if generic != self.generics.entity:
                        error(
                            f"only {self.generics.entity.name} is the only"
                            " legal generic in this context"
                        )
                    with self.ctx.lkt_context(type_args):
                        check_source_language(
                            len(type_args) == 1,
                            f"{generic.name} expects one type argument:"
                            " the node type"
                        )

                    # TODO: ensure that node_type is indeed a node type
                    node_type = self.resolve_type(type_args[0], env)
                    return lower_new(node_type.entity)

                # Otherwise the call has to be a dot expression, for a method
                # invocation.
                elif not isinstance(call_name, L.BaseDotExpr):
                    with self.ctx.lkt_context(call_name):
                        error("invalid call prefix")

                null_cond = isinstance(call_name, L.NullCondDottedName)

                # TODO: introduce a pre-lowering pass to extract the list of
                # types and their fields/methods so that we can perform
                # validation here.
                method_prefix = lower(call_name.f_prefix)
                method_name = call_name.f_suffix.text

                # If the method call is protected by a null conditional,
                # prepare when we need to build the Then expression.
                if null_cond:
                    null_cond_var = AbstractVariable(
                        names.Name("Var_Expr"), create_local=True,
                    )
                    then_prefix = method_prefix
                    method_prefix = null_cond_var

                if method_name in ("all", "any"):
                    lambda_info = extract_lambda_and_kwargs(
                        call_expr, collection_iter_signature, 1, 2
                    )
                    element_arg, index_arg = lambda_info.largs
                    assert element_arg is not None

                    element_var = var_for_lambda_arg(
                        lambda_info.scope, element_arg, 'item'
                    )
                    index_var = (
                        None
                        if index_arg is None else
                        var_for_lambda_arg(
                            lambda_info.scope, index_arg, 'index', T.Int
                        )
                    )

                    # Finally lower the expressions
                    inner_expr = self.lower_expr(
                        lambda_info.expr, lambda_info.scope, local_vars
                    )
                    result = E.Quantifier.create_expanded(
                        method_name,
                        method_prefix,
                        inner_expr,
                        element_var,
                        index_var,
                    )

                elif method_name == "as_array":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.as_array

                elif method_name == "as_int":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.as_int

                elif method_name == "do":
                    lambda_info = extract_lambda_and_kwargs(
                        call_expr, do_signature, 1, 1
                    )
                    arg_node = lambda_info.largs[0]
                    assert arg_node is not None

                    arg_var = var_for_lambda_arg(
                        lambda_info.scope,
                        arg_node,
                        "var_expr",
                        create_local=True,
                    )
                    then_expr = self.lower_expr(
                        lambda_info.expr, lambda_info.scope, local_vars
                    )

                    default_val = (
                        lower(lambda_info.kwargs["default_val"])
                        if "default_val" in lambda_info.kwargs else
                        None
                    )

                    result = E.Then.create_from_exprs(
                        method_prefix,
                        then_expr,
                        arg_var,
                        default_val,
                    )

                elif method_name == "filter":
                    lambda_info = extract_lambda_and_kwargs(
                        call_expr, collection_iter_signature, 1, 2
                    )
                    element_arg, index_arg = lambda_info.largs
                    assert element_arg is not None

                    element_var = var_for_lambda_arg(
                        lambda_info.scope, element_arg, "item"
                    )
                    index_var = (
                        None
                        if index_arg is None else
                        var_for_lambda_arg(
                            lambda_info.scope, index_arg, "index", T.Int
                        )
                    )

                    # Finally lower the expressions
                    inner_expr = self.lower_expr(
                        lambda_info.expr, lambda_info.scope, local_vars
                    )
                    result = E.Map.create_expanded(
                        method_prefix,
                        element_var,
                        element_var,
                        index_var,
                        inner_expr,
                    )

                elif method_name == "filtermap":
                    # Validate arguments for ".filtermap()" itself
                    parsed_args = filtermap_signature.match(
                        self.ctx, call_expr
                    )
                    for arg in parsed_args.positional_args:
                        if not isinstance(arg, L.LambdaExpr):
                            with self.ctx.lkt_context(arg):
                                error("lambda expressions expceted")

                    # Validate and analyze the two lambda expressions
                    lambda_0 = parsed_args.positional_args[0]
                    assert isinstance(lambda_0, L.LambdaExpr)
                    map_scope, map_args, map_body = extract_lambda(
                        lambda_0, 1, 2
                    )
                    lambda_1 = parsed_args.positional_args[1]
                    assert isinstance(lambda_1, L.LambdaExpr)
                    filter_scope, filter_args, filter_body = extract_lambda(
                        lambda_1, 1, 2
                    )

                    # We need to have two different scopes for the two lambda
                    # expressions, but need to create common iteration
                    # variables for both. The "element" variable is always
                    # present, but the "fitler" one may be present in one, two
                    # or no lambda expression.
                    assert map_args[0] is not None
                    assert filter_args[0] is not None
                    element_var = var_for_lambda_arg(
                        map_scope, map_args[0], "item"
                    )
                    name_from_lower(
                        self.ctx, "argument", filter_args[0].f_syn_name
                    )
                    add_lambda_arg_to_scope(
                        filter_scope, filter_args[0], element_var
                    )

                    index_var = None
                    if map_args[1] is not None:
                        index_var = var_for_lambda_arg(
                            map_scope, map_args[1], "index", T.Int
                        )
                        if filter_args[1] is not None:
                            name_from_lower(
                                self.ctx, "argument", filter_args[1].f_syn_name
                            )
                            add_lambda_arg_to_scope(
                                filter_scope, filter_args[1], index_var
                            )
                    elif filter_args[1] is not None:
                        index_var = var_for_lambda_arg(
                            filter_scope, filter_args[1], "Index", T.Int
                        )

                    # Lower their expressions
                    map_expr = self.lower_expr(
                        map_body, map_scope, local_vars
                    )
                    filter_expr = self.lower_expr(
                        filter_body, filter_scope, local_vars
                    )

                    return E.Map.create_expanded(
                        collection=method_prefix,
                        expr=map_expr,
                        element_var=element_var,
                        index_var=index_var,
                        filter_expr=filter_expr,
                    )

                elif method_name == "find":
                    lambda_info = extract_lambda_and_kwargs(
                        call_expr, collection_iter_signature, 1, 1
                    )
                    elt_arg = lambda_info.largs[0]
                    assert elt_arg is not None

                    elt_var = var_for_lambda_arg(
                        lambda_info.scope, elt_arg, 'item'
                    )
                    inner_expr = self.lower_expr(
                        lambda_info.expr, lambda_info.scope, local_vars
                    )

                    result = E.Find.create_expanded(
                        method_prefix, inner_expr, elt_var, index_var=None
                    )

                elif method_name == "get_value":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.get_value

                elif method_name == "length":
                    empty_signature.match(self.ctx, call_expr)
                    result = getattr(method_prefix, "length")

                elif method_name in ("map", "mapcat"):
                    lambda_info = extract_lambda_and_kwargs(
                        call_expr, collection_iter_signature, 1, 2
                    )
                    element_arg, index_arg = lambda_info.largs
                    assert element_arg is not None

                    element_var = var_for_lambda_arg(
                        lambda_info.scope, element_arg, 'item'
                    )
                    index_var = (
                        None
                        if index_arg is None else
                        var_for_lambda_arg(
                            lambda_info.scope, index_arg, 'index', T.Int
                        )
                    )

                    # Finally lower the expressions
                    inner_expr = self.lower_expr(
                        lambda_info.expr, lambda_info.scope, local_vars
                    )
                    result = E.Map.create_expanded(
                        method_prefix,
                        inner_expr,
                        element_var,
                        index_var,
                        do_concat=method_name == "mapcat",
                    )

                elif method_name == "singleton":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.singleton

                elif method_name == "solve":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.solve

                elif method_name == "take_while":
                    lambda_info = extract_lambda_and_kwargs(
                        call_expr, collection_iter_signature, 1, 2
                    )
                    element_arg, index_arg = lambda_info.largs
                    assert element_arg is not None

                    element_var = var_for_lambda_arg(
                        lambda_info.scope, element_arg, 'item'
                    )
                    index_var = (
                        None
                        if index_arg is None else
                        var_for_lambda_arg(
                            lambda_info.scope, index_arg, "index", T.Int
                        )
                    )
                    inner_expr = self.lower_expr(
                        lambda_info.expr, lambda_info.scope, local_vars
                    )
                    result = E.Map.create_expanded(
                        method_prefix,
                        element_var,
                        element_var,
                        index_var,
                        take_while_expr=inner_expr,
                    )

                elif method_name == "to_symbol":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.to_string  # type: ignore

                elif method_name == "unique":
                    empty_signature.match(self.ctx, call_expr)
                    result = method_prefix.unique

                else:
                    # Otherwise, this call must be a method invocation. Note
                    # that not all methods map to actual field access in the
                    # generated code. For instance, calls to the String.join
                    # built-in method are turned into Join instances, so the
                    # "callee" variable below is not necessarily a FieldAccess
                    # instance.
                    args, kwargs = lower_args()
                    result = getattr(method_prefix, method_name)
                    result = result(*args, **kwargs)

                return (
                    E.Then.create_from_exprs(
                        base=then_prefix,
                        then_expr=result,
                        var_expr=null_cond_var,
                    )
                    if null_cond else
                    result
                )

            elif isinstance(expr, L.CastExpr):
                subexpr = lower(expr.f_expr)
                excludes_null = expr.f_excludes_null.p_as_bool
                dest_type = self.resolve_type(expr.f_dest_type, env)
                return Cast(subexpr, dest_type, do_raise=excludes_null)

            elif isinstance(expr, L.CharLit):
                return E.CharacterLiteral(expr.p_denoted_value)

            elif isinstance(expr, L.BaseDotExpr):
                null_cond = isinstance(expr, L.NullCondDottedName)

                # Dotted expressions can designate an enum value (if the prefix
                # is a type name) or a member access.
                prefix_node = expr.f_prefix
                if isinstance(prefix_node, L.RefId):
                    try:
                        t = env.lookup(prefix_node.text)
                    except KeyError:
                        pass
                    else:
                        if isinstance(t, (Scope.BuiltinType, Scope.UserType)):
                            with self.ctx.lkt_context(expr.f_suffix):
                                check_source_language(
                                    not null_cond,
                                    "null-conditional dotted name notation is"
                                    " illegal to designate an enum value",
                                )
                            # The suffix refers to the declaration of en enum
                            # value: the prefix must designate the
                            # corresponding enum type.
                            return getattr(t.defer, expr.f_suffix.text)

                # Otherwise, the prefix is a regular expression, so this dotted
                # expression is an access to a member.
                prefix = lower(expr.f_prefix)
                suffix = expr.f_suffix.text
                if null_cond:
                    arg_var = AbstractVariable(
                        names.Name("Var_Expr"), create_local=True
                    )
                    return E.Then.create_from_exprs(
                        base=prefix,
                        then_expr=getattr(arg_var, suffix),
                        var_expr=arg_var,
                    )
                else:
                    return getattr(prefix, suffix)

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
                conditions = (
                    [(expr.f_cond_expr, expr.f_then_expr)]
                    + [
                        (alt.f_cond_expr, alt.f_then_expr)
                        for alt in expr.f_alternatives
                    ]
                )
                for c, e in reversed(conditions):
                    result = E.If(lower(c), lower(e), result)
                return result

            elif isinstance(expr, L.Isa):
                subexpr = lower(expr.f_expr)
                nodes = [
                    self.resolve_type(type_ref, env)
                    for type_ref in expr.f_dest_type
                ]
                return E.IsA(subexpr, *nodes)

            elif isinstance(expr, L.LogicExpr):
                expr = expr.f_expr
                if isinstance(expr, L.RefId):
                    if expr.text == "true":
                        return E.LogicTrue()
                    elif expr.text == "false":
                        return E.LogicFalse()

                elif isinstance(expr, L.CallExpr):
                    call_name = expr.f_name
                    if not isinstance(call_name, L.RefId):
                        with self.ctx.lkt_context(expr):
                            error("invalid logic expression")

                    if call_name.text in ("all", "any"):
                        parsed_args = logic_all_any_signature.match(
                            self.ctx, expr
                        )
                        eq_array_expr = lower(parsed_args.positional_args[0])
                        return (
                            ELogic.All(eq_array_expr)
                            if call_name.text == "all" else
                            ELogic.Any(eq_array_expr)
                        )

                    elif call_name.text == "domain":
                        parsed_args = domain_signature.match(self.ctx, expr)
                        logic_var = lower(parsed_args.positional_args[0])
                        domain_expr = lower(parsed_args.positional_args[1])
                        return logic_var.domain(domain_expr)

                    elif call_name.text == "eq":
                        parsed_args = eq_signature.match(self.ctx, expr)
                        from_expr = lower(parsed_args.positional_args[0])
                        to_expr = lower(parsed_args.positional_args[1])
                        conv_prop = (
                            self.resolve_property(
                                parsed_args.keyword_args["conv_prop"],
                            )
                            if "conv_prop" in parsed_args.keyword_args else
                            None
                        )
                        return E.Bind(from_expr, to_expr, conv_prop=conv_prop)

                    elif call_name.text == "predicate":
                        parsed_args = predicate_signature.match(self.ctx, expr)
                        pred_prop = self.resolve_property(
                            parsed_args.positional_args[0]
                        )
                        node_expr = lower(parsed_args.positional_args[1])
                        arg_exprs = [
                            lower(arg) for arg in parsed_args.other_args
                        ]
                        return E.Predicate(pred_prop, node_expr, *arg_exprs)

                    elif call_name.text == "propagate":
                        parsed_args = propagate_signature.match(self.ctx, expr)
                        dest_var = lower(parsed_args.positional_args[0])
                        comb_prop = self.resolve_property(
                            parsed_args.positional_args[1]
                        )
                        arg_vars = [
                            lower(arg) for arg in parsed_args.other_args
                        ]
                        return E.NPropagate(dest_var, comb_prop, *arg_vars)

                with self.ctx.lkt_context(expr):
                    error("invalid logic expression")

            elif isinstance(expr, L.MatchExpr):
                assert local_vars is not None

                prefix_expr = lower(expr.f_match_expr)

                # Lower each individual matcher
                matchers: list[
                    tuple[TypeRepo.Defer, AbstractVariable, AbstractExpression]
                ] = []
                for i, m in enumerate(expr.f_branches):
                    # Make sure the identifier has the expected casing
                    decl_id = m.f_decl.f_syn_name
                    if decl_id.text != "_":
                        with self.ctx.lkt_context(decl_id):
                            names.Name.check_from_lower(decl_id.text)

                    # Fetch the type to match, if any
                    syn_type = m.f_decl.f_decl_type
                    matched_type = (
                        None
                        if syn_type is None else
                        resolve_type(self.resolve_type(syn_type, env))
                    )

                    # Create the match variable
                    var_name = names.Name(f"Match_{i}")
                    with AbstractExpression.with_location(
                        Location.from_lkt_node(m.f_decl)
                    ):
                        match_var = AbstractVariable(
                            name=var_name,
                            type=matched_type,
                            source_name=decl_id.text,
                        )
                    match_var.local_var = local_vars.create_scopeless(
                        var_name, matched_type
                    )

                    # Lower the matcher expression, making the match variable
                    # available if intended.
                    loc = Location.from_lkt_node(m)
                    sub_env = env.create_child(
                        f"scope for match branch at {loc.gnu_style_repr()}"
                    )
                    if decl_id.text != "_":
                        sub_env.add(
                            Scope.UserValue(decl_id.text, m.f_decl, match_var)
                        )
                    match_expr = self.lower_expr(m.f_expr, sub_env, local_vars)

                    matchers.append((matched_type, match_var, match_expr))

                result = E.Match(prefix_expr)
                result.matchers = matchers
                return result

            elif isinstance(expr, L.NotExpr):
                return E.Not(lower(expr.f_expr))

            elif isinstance(expr, L.NullLit):
                result_type = self.resolve_type(expr.f_dest_type, env)
                return E.No(result_type)

            elif isinstance(expr, L.NumLit):
                return E.Literal(int(expr.text))

            elif isinstance(expr, L.ParenExpr):
                return lower(expr.f_expr)

            elif isinstance(expr, L.RaiseExpr):
                # A raise expression can only contain a PropertyError struct
                # constructor.
                cons_expr = expr.f_except_expr
                if not isinstance(cons_expr, L.CallExpr):
                    error("'raise' must be followed by a call expression")
                call_name = cons_expr.f_name
                entity = self.resolve_entity(call_name, env)
                if not isinstance(entity, Scope.Exception):
                    error(f"exception expected, got {entity.diagnostic_name}")

                # Get the exception message argument
                args_nodes, kwargs_nodes = extract_call_args(cons_expr)
                msg_expr: Optional[L.Expr] = None
                if args_nodes:
                    msg_expr = args_nodes.pop()
                elif kwargs_nodes:
                    msg_expr = kwargs_nodes.pop("exception_message")
                with self.ctx.lkt_context(cons_expr.f_args):
                    check_source_language(
                        not args_nodes and not kwargs_nodes,
                        "at most one argument expected: the exception message",
                    )

                if msg_expr is None:
                    msg = "PropertyError exception"
                else:
                    # TODO (S321-013): handle dynamic error message
                    if not isinstance(msg_expr, L.StringLit):
                        with self.ctx.lkt_context(msg_expr):
                            error("string literal expected")
                    msg = msg_expr.p_denoted_value

                return entity.constructor(
                    self.resolve_type(expr.f_dest_type, env), msg
                )

            elif isinstance(expr, L.RefId):
                entity = self.resolve_entity(expr, env)
                if isinstance(entity, Scope.BuiltinValue):
                    return entity.value
                elif isinstance(entity, Scope.UserValue):
                    return entity.variable
                else:
                    with self.ctx.lkt_context(expr):
                        if isinstance(entity, Scope.DynVar):
                            error(
                                f"{entity.name} is not bound in this context:"
                                " please use the 'bind' construct to bind is"
                                " first."
                            )
                        else:
                            error(
                                f"value expected, got {entity.diagnostic_name}"
                            )

            elif isinstance(expr, L.StringLit):
                string_prefix = expr.p_prefix
                string_value = expr.p_denoted_value
                if string_prefix == "\x00":
                    return E.String(string_value)
                elif string_prefix == "s":
                    return E.SymbolLiteral(string_value)
                else:
                    error("invalid string prefix")

            elif isinstance(expr, L.SubscriptExpr):
                null_cond = isinstance(expr, L.NullCondSubscriptExpr)
                prefix = lower(expr.f_prefix)
                index = lower(expr.f_index)
                return (
                    prefix.at(index)
                    if isinstance(expr, L.NullCondSubscriptExpr) else
                    prefix.at_or_raise(index)
                )

            elif isinstance(expr, L.TryExpr):
                return E.Try(
                    try_expr=lower(expr.f_try_expr),
                    else_expr=(
                        None
                        if expr.f_or_expr is None
                        else lower(expr.f_or_expr)
                    ),
                )

            else:
                assert False, 'Unhandled expression: {}'.format(expr)

        return lower(expr)

    def lower_property_arguments(
        self,
        prop: PropertyDef,
        arg_decl_list: Optional[L.FunArgDeclList],
        label: str,
    ) -> tuple[list[L.FunArgDecl], Scope]:
        """
        Lower a property's arguments and create the root scope used to lower
        the property's root expression.
        """
        arguments: list[L.FunArgDecl] = []
        scope = self.root_scope.create_child(f"scope for {label}")

        # Lower arguments and register them both in the property's argument
        # list and in the root property scope.
        for a in arg_decl_list or []:
            arguments.append(a)

            annotations = parse_annotations(
                self.ctx,
                FunArgAnnotations,
                a.f_decl_annotations,
                self.root_scope,
            )

            source_name = a.f_syn_name.text
            reserved = PropertyDef.reserved_arg_lower_names
            with self.ctx.lkt_context(a.f_syn_name):
                check_source_language(
                    source_name not in reserved,
                    "Arguments cannot have reserved names ({})".format(
                        ", ".join(reserved)
                    ),
                )
            with AbstractExpression.with_location(Location.from_lkt_node(a)):
                arg = Argument(
                    name=name_from_lower(self.ctx, "argument", a.f_syn_name),
                    type=self.resolve_type(a.f_decl_type, scope),
                    source_name=source_name,
                )
            if annotations.ignored:
                arg.var.tag_ignored()
            prop.arguments.append(arg)
            scope.add(Scope.Argument(source_name, a, arg.var))

        return arguments, scope

    def lower_property(
        self,
        full_decl: L.FullDecl,
        node_name: str,
    ) -> PropertyDef:
        """
        Lower the property described in ``decl``.
        """
        decl = full_decl.f_decl
        assert isinstance(decl, L.FunDecl)
        annotations = parse_annotations(
            self.ctx, FunAnnotations, full_decl, self.root_scope,
        )
        return_type = self.resolve_type(decl.f_return_type, self.root_scope)

        # If @uses_entity_info and @uses_envs are not present for non-external
        # properties, use None instead of False, for the validation machinery
        # in PropertyDef to work properly (we expect False/true for external
        # properties, and None for non-external ones).
        uses_entity_info: Optional[bool]
        uses_envs: Optional[bool]
        if annotations.external:
            uses_entity_info = annotations.uses_entity_info
            uses_envs = annotations.uses_envs
        else:
            uses_entity_info = annotations.uses_entity_info or None
            uses_envs = annotations.uses_envs or None

        # Create the property to return
        result = PropertyDef(
            expr=None,
            prefix=AbstractNodeData.PREFIX_PROPERTY,
            doc=self.ctx.lkt_doc(decl),

            # When the @export annotation is missing, use "None" to mean
            # "public status unspecified", as the property can still be public
            # thanks to inheritance.
            public=annotations.export or None,

            abstract=annotations.abstract,
            type=return_type,
            abstract_runtime_check=False,
            memoized=annotations.memoized,
            call_memoizable=annotations.call_memoizable,
            memoize_in_populate=False,
            external=annotations.external,
            uses_entity_info=uses_entity_info,
            uses_envs=uses_envs,
            optional_entity_info=False,
            warn_on_unused=not annotations.ignore_unused,
            ignore_warn_on_node=None,
            call_non_memoizable_because=(
                annotations.call_non_memoizable_because
            ),
            activate_tracing=annotations.trace,
            dump_ir=False,
            lazy_field=False,
            final=annotations.final,
        )
        result._doc_location = Location.from_lkt_node(full_decl.f_doc)

        # Lower its arguments
        arguments, scope = self.lower_property_arguments(
            result, decl.f_args, f"property {node_name}.{decl.f_syn_name.text}"
        )

        # Keep track of the requested set of dynamic variables
        dynvars: list[
            tuple[E.DynamicVariable, L.Expr | None]
        ] | None = None
        if annotations.with_dynvars is not None:
            dynvars = []
            for dynvar, init_expr in annotations.with_dynvars:
                dynvars.append((dynvar.variable, init_expr))
                scope.add(
                    Scope.LocalVariable(
                        dynvar.name, dynvar.decl, dynvar.variable
                    )
                )

        # Plan to lower its expressions later
        self.properties_to_lower.append(
            self.PropertyToLower(result, arguments, dynvars)
            if decl.f_body is None else
            self.PropertyAndExprToLower(
                result, arguments, dynvars, decl.f_body, scope
            )
        )

        return result

    def lower_fields(
        self,
        decls: L.DeclBlock,
        allowed_field_types: Tuple[Type[AbstractNodeData], ...],
        user_field_public: bool,
        struct_name: str,
    ) -> List[Tuple[names.Name, AbstractNodeData]]:
        """
        Lower the fields described in the given DeclBlock node.

        :param decls: Declarations to process.
        :param allowed_field_types: Set of types allowed for the fields to
            load.
        :param user_field_public: Whether user fields should be made public.
        """
        result = []
        for full_decl in decls:
            with self.ctx.lkt_context(full_decl):
                decl = full_decl.f_decl
                name = name_from_lower(self.ctx, "field", decl.f_syn_name)

                field: AbstractNodeData

                if isinstance(decl, L.FunDecl):
                    check_source_language(
                        any(issubclass(PropertyDef, cls)
                            for cls in allowed_field_types),
                        'Properties not allowed in this context'
                    )
                    field = self.lower_property(full_decl, struct_name)
                else:
                    field = self.lower_base_field(
                        full_decl,
                        allowed_field_types,
                        user_field_public,
                        struct_name,
                    )

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

        # Check the set of traits that this node implements
        node_trait_ref: L.LktNode | None = None
        token_node_trait_ref: L.LktNode | None = None
        error_node_trait_ref: L.LktNode | None = None
        for trait_ref in decl.f_traits:
            if trait_ref.text == "TokenNode":
                token_node_trait_ref = trait_ref

            elif trait_ref.text == "ErrorNode":
                error_node_trait_ref = trait_ref

            else:
                if not isinstance(trait_ref, L.GenericTypeRef):
                    with self.ctx.lkt_context(trait_ref):
                        error("Nodes cannot implement this trait")

                # This is a generic instantiation
                generic_trait = trait_ref.f_type_name
                type_args = list(trait_ref.f_params)
                if generic_trait.text == "Node":
                    # If this trait is an instantiation of the Node trait, make
                    # sure it is instantiated on the root node itself (i.e.
                    # "decl").
                    with self.ctx.lkt_context(trait_ref):
                        decl_name = decl.f_syn_name.text
                        check_source_language(
                            len(type_args) == 1
                            and type_args[0].text == decl_name,
                            "The Node generic trait must be instantiated with"
                            f" the root node ({decl_name})",
                        )
                    node_trait_ref = trait_ref

                else:
                    with self.ctx.lkt_context(trait_ref):
                        error("Nodes cannot implement this trait")

        def check_trait(trait_ref: Optional[L.LktNode],
                        expected: bool,
                        message: str) -> None:
            """
            If ``expected`` is ``True``, emit an error if ``trait_ref`` is
            ``None``. If ``expected`` is ``False``, emit an error if
            ``trait_ref`` is not ``None``. In both cases, use ``message`` as
            the error message.
            """
            if expected:
                check_source_language(trait_ref is not None, message)
            elif trait_ref is not None:
                with self.ctx.lkt_context(trait_ref):
                    error(message)

        # Root node case
        base_type_node = decl.p_base_type
        if base_type_node is None:
            check_trait(
                node_trait_ref,
                True,
                "The root node must implement the Node trait"
            )
            check_trait(
                token_node_trait_ref,
                False,
                "The root node cannot be a token node"
            )
            check_trait(
                error_node_trait_ref,
                False,
                "The root node cannot be an error node"
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
            base_type = self.resolve_and_lower_node(
                base_type_node, self.root_scope
            )

            check_trait(
                node_trait_ref,
                False,
                "Only the root node can implement the Node trait"
            )

            # This is a token node if either the TokenNode trait is implemented
            # or if the base node is a token node itself. Likewise for
            # ErrorNode.
            is_token_node = token_node_trait_ref is not None
            is_error_node = error_node_trait_ref is not None

            check_source_language(
                base_type is not base_type.is_enum_node,
                'Inheritting from an enum node is forbidden'
            )

        with self.ctx.lkt_context(error_node_trait_ref):
            # Determine whether this node is abstract. Remember that base enum
            # node types are abstract (it is their derivations that are
            # concrete).
            is_abstract = (
                not isinstance(annotations, NodeAnnotations)
                or annotations.abstract
            )
            if is_abstract and is_error_node:
                error("Error nodes cannot be abstract")

            # Determine whether this node is synthetic
            is_synthetic = annotations.synthetic
            if is_synthetic and is_error_node:
                error("Error nodes cannot be synthetic")

            if base_type and base_type.is_list and is_error_node:
                error("Error nodes cannot be lists")

            if is_token_node and is_error_node:
                error("Error nodes cannot be token nodes")

        # Lower fields. Regular nodes can hold all types of fields, but token
        # nodes and enum nodes can hold only user field and properties.
        allowed_field_types = (
            (UserField, PropertyDef)
            if is_token_node or is_enum_node
            else (AbstractNodeData, )
        )
        fields = self.lower_fields(
            decl.f_decls,
            allowed_field_types,
            user_field_public=False,
            struct_name=decl.f_syn_name.text,
        )

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
            name_from_camel(self.ctx, "node type", decl.f_syn_name),
            location=loc,
            doc=self.ctx.lkt_doc(decl),
            base=base_type,
            fields=fields,
            is_abstract=is_abstract,
            is_token_node=is_token_node,
            is_error_node=is_error_node,
            is_synthetic=is_synthetic,
            has_abstract_list=annotations.has_abstract_list,
            is_enum_node=is_enum_node,
            is_bool_node=is_bool_node,
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node(decl.parent.f_doc)

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

        # Reject non-null fields for error nodes. Non-null fields can come from
        # this node's own declaration, or they can come from inheritance.
        if is_error_node:
            error_msg = "Error nodes can only have null fields"
            for f in result.get_parse_fields(include_inherited=True):
                if not (f.null or f.abstract):
                    if f.struct != result:
                        error(f"{error_msg}: {f.qualname} is not null")
                    else:
                        with f.diagnostic_context:
                            error(error_msg)

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
                EnumNodeAlternative(
                    name_from_camel(
                        self.ctx,
                        "enum node alternative",
                        alt.f_syn_name,
                    ),
                    enum_node,
                    None,
                    Location.from_lkt_node(alt)
                )
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
            name = name_from_lower(self.ctx, "enum value", lit.f_syn_name)
            check_source_language(
                name not in value_names,
                'The "{}" literal is present twice'
            )
            value_names.append(name)

        # If present, validate the default value
        default_value: names.Name | None = None
        default_expr = annotations.with_default
        if default_expr is not None:
            with self.ctx.lkt_context(default_expr):
                if not isinstance(default_expr, L.RefId):
                    error("enum value identifier expected")
                default_value = names.Name.from_lower(default_expr.text)
                if default_value not in value_names:
                    error("no such value in this enum")

        result = EnumType(
            name=name_from_camel(self.ctx, "enum type", decl.f_syn_name),
            location=Location.from_lkt_node(decl),
            doc=self.ctx.lkt_doc(decl),
            value_names=value_names,
            default_val_name=default_value,
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node(decl.parent.f_doc)
        return result

    def create_struct(self,
                      decl: L.StructDecl,
                      annotations: StructAnnotations) -> StructType:
        """
        Create a StructType instance.

        :param decl: Corresponding declaration node.
        :param annotations: Annotations for this declaration.
        """
        name = decl.f_syn_name.text

        allowed_field_types: Tuple[Type[AbstractNodeData], ...]
        if annotations.metadata:
            allowed_field_types = (MetadataField, )
            check_source_language(
                CompiledTypeRepo.env_metadata is None,
                "Only one struct can be the env metadata",
            )
        else:
            allowed_field_types = (UserField, )

        fields = self.lower_fields(
            decl.f_decls,
            allowed_field_types,
            user_field_public=True,
            struct_name=name,
        )

        result = StructType(
            name_from_camel(self.ctx, "struct type", decl.f_syn_name),
            location=Location.from_lkt_node(decl),
            doc=self.ctx.lkt_doc(decl),
            fields=fields,
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node(decl.parent.f_doc)
        if annotations.metadata:
            CompiledTypeRepo.env_metadata = result
        return result


def create_types(ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]) -> None:
    """
    Create types from Lktlang units.

    :param ctx: Context in which to create these types.
    :param lkt_units: Non-empty list of analysis units where to look for type
        declarations.
    """
    LktTypesLoader(ctx, lkt_units)
