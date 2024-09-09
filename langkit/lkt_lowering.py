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

* [ROOT_SCOPE_CREATION] The first step looks at all top-level declarations
  (lexers, grammars and types) and registers them by name in the root scope.

* [DYNVAR_LOWERING] At this point, another step lowers all dynamic variables.

* [TYPES_LOWERING] We then iterate on all types and lower them. All type
  declarations in the language spec are lowered in sequence (arbitrary order),
  except base classes, which are lowered before classes that they derive from.
  This step creates the actual ``CompiledType`` instances as well as the
  ``AbstractNodeData`` ones.

* [TYPES_RESOLUTION] Now that we have a ``CompiledType`` for all builtin types
  and all types declared in the language spec, it is possible to resolve all
  type references: we iterate on all user types to replace type references
  (``TypeRepo.defer``) instances and replace them with the corresponding
  ``CompiledType`` instances.

* [EXPR_LOWERING] The arguments' default values and the bodies of all
  properties are lowered.

* [ENV_SPECS_LOWERING] Finally, env specs are lowered.
"""

from __future__ import annotations

import abc
from collections import OrderedDict
from dataclasses import dataclass
import enum
import itertools
import os.path
from typing import (
    Any, Callable, ClassVar, Dict, Generic, List, Optional, Set, Tuple, Type,
    TypeVar, Union, cast, overload
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
import langkit.dsl
from langkit.envs import (
    AddEnv, AddToEnv, Do, EnvAction, EnvSpec, HandleChildren, RefEnvs, RefKind,
    SetInitialEnv
)
import langkit.expressions as E
from langkit.expressions import (
    AbstractExpression, AbstractKind, AbstractProperty, AbstractVariable, Cast,
    Let, LocalVars, NullCond, Property, PropertyDef, create_lazy_field, unsugar
)
import langkit.expressions.logic as ELogic
from langkit.lexer import (
    Action, Alt, Case, Ignore, Lexer, LexerToken, Literal, Matcher, NoCaseLit,
    Pattern, RuleAssoc, TokenAction, TokenFamily, WithSymbol, WithText,
    WithTrivia
)
import langkit.names as names
from langkit.parsers import (
    Cut, Defer, Discard, DontSkip, Grammar, List as PList, ListSepExtra, Null,
    Opt, Or, Parser, Pick, Predicate, Skip, StopCut, _Row, _Token, _Transform
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


def check_no_decoding_error(
    node: L.LktNode,
    result: L.DecodedCharValue | L.DecodedStringValue
) -> None:
    """
    If ``result`` has an error, stop with the corresponding diagnostic.
    """
    if result.has_error:
        filename = node.unit.filename
        line = result.error_sloc.line
        column = result.error_sloc.column
        with diagnostic_context(Location(filename, line, column)):
            error(result.error_message)


def denoted_str(strlit: L.StringLit | L.TokenLit | L.TokenPatternLit) -> str:
    """
    Return the string value that this literal denotes.
    """
    result = strlit.p_denoted_value
    check_no_decoding_error(strlit, result)
    return result.value


def denoted_char(charlit: L.CharLit) -> str:
    """
    Return the character value that this literal denotes.
    """
    result = charlit.p_denoted_value
    check_no_decoding_error(charlit, result)
    return result.value


def parse_static_str(ctx: CompileCtx, expr: L.Expr) -> str:
    """
    Return the string value that this expression denotes.
    """
    with ctx.lkt_context(expr):
        if not isinstance(expr, L.StringLit) or expr.p_is_prefixed_string:
            error("simple string literal expected")
        return denoted_str(expr)


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
    ctx = L.AnalysisContext(unit_provider=L.UnitProvider.from_lkt_path())
    process_unit(
        ctx.get_from_file(lkt_file)
    )

    # Forward potential lexing/parsing errors to our diagnostics system
    for u, d in diagnostics:
        with diagnostic_context(Location.from_sloc_range(u, d.sloc_range)):
            non_blocking_error(d.message)
    errors_checkpoint()
    return list(units_map.values())


class Scope:
    """
    Scope data structure, use to resolve named references.
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

    class BuiltinEntity(Entity):
        """
        Any entity that is created automatically by Lkt.
        """
        pass

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

    class Generic(BuiltinEntity):
        """
        Generic declaration, always created automatically by Lkt.
        """

        @property
        def diagnostic_name(self) -> str:
            return f"the generic {self.name}"

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
    class RefKindValue(BuiltinEntity):
        """
        Reference kinds in "reference()" env actions.
        """

        value: RefKind

        @property
        def diagnostic_name(self) -> str:
            return f"the reference kind {self.name}"

    @dataclass
    class UserEntity(Entity):
        """
        Entity defined in user code.
        """

        diagnostic_node: L.LktNode
        """
        Lkt node to use as the reference for this entity when creating
        diagnostics: generally a declaration (VarDecl for a variable
        declaration, TypeDecl for a type declaration, ...), sometimes not
        (VarBind for a bound dynamic variable).
        """

        kind_name: ClassVar[str]
        """
        Name for the kind of this entity, to use when formatting diagnostics.
        """

        @property
        def diagnostic_name(self) -> str:
            loc = Location.from_lkt_node(self.diagnostic_node)
            return (
                f"the {self.kind_name} {self.name} at {loc.gnu_style_repr()}"
            )

    class Lexer(UserEntity):
        """
        Lexer declaration.
        """

        kind_name = "lexer"

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

    class LocalVariable(UserValue):
        """
        Local variable declaration.
        """

        kind_name = "local variable"

    @dataclass
    class BoundDynVar(LocalVariable):
        """
        Dynamic variable that has been bound.

        ``BoudDynVar`` instances are put in a scope as soon as a dynamic
        variable is bound in that scope: it allows keep track of the fact that
        it is bound (to be used as regular variables, hence the
        ``LocalVariable`` derivation).
        """

        kind_name = "bound dynamic variable"

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
        # Pseudo "_" entities are used to mean "do not bind this to an
        # identifier": just skip them.
        if entity.name == "_":
            return

        other_entity = self.mapping.get(entity.name)
        if other_entity is None:
            self.mapping[entity.name] = entity
        else:
            with self.context.lkt_context(entity.diagnostic_node):
                other_label = (
                    other_entity.diagnostic_name
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


def create_root_scope(ctx: CompileCtx) -> Scope:
    """
    Create and return a root scope.

    TODO (eng/libadalang/langkit#704): once the DSL is no more, use the same
    root scope in:

    * the lexer lowering pass,
    * the grammar lowering pass,
    * the types lowering pass.

    See callers for this helper function.
    """
    return Scope("the root scope", ctx)


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


class BuiltinAttribute(enum.Enum):
    as_bare_entity = enum.auto()
    as_entity = enum.auto()
    children = enum.auto()
    env_node = enum.auto()
    env_parent = enum.auto()
    is_null = enum.auto()
    parent = enum.auto()
    rebindings_new_env = enum.auto()
    rebindings_old_env = enum.auto()
    rebindings_parent = enum.auto()
    root = enum.auto()
    symbol = enum.auto()
    to_symbol = enum.auto()


class BuiltinMethod(enum.Enum):
    all = enum.auto()
    any = enum.auto()
    append_rebinding = enum.auto()
    as_array = enum.auto()
    as_big_int = enum.auto()
    as_int = enum.auto()
    concat_rebindings = enum.auto()
    contains = enum.auto()
    do = enum.auto()
    empty = enum.auto()
    env_group = enum.auto()
    env_orphan = enum.auto()
    filter = enum.auto()
    filtermap = enum.auto()
    find = enum.auto()
    get = enum.auto()
    get_first = enum.auto()
    get_value = enum.auto()
    iall = enum.auto()
    iany = enum.auto()
    ifilter = enum.auto()
    ifiltermap = enum.auto()
    ilogic_all = enum.auto()
    ilogic_any = enum.auto()
    imap = enum.auto()
    imapcat = enum.auto()
    is_visible_from = enum.auto()
    itake_while = enum.auto()
    join = enum.auto()
    length = enum.auto()
    logic_all = enum.auto()
    logic_any = enum.auto()
    map = enum.auto()
    mapcat = enum.auto()
    rebind_env = enum.auto()
    shed_rebindings = enum.auto()
    singleton = enum.auto()
    solve = enum.auto()
    solve_with_diagnostics = enum.auto()
    super = enum.auto()
    take_while = enum.auto()
    to_builder = enum.auto()
    unique = enum.auto()
    update = enum.auto()


@dataclass
class FieldKinds:
    """
    Set of field kinds. Used to filter what kind of fields are legal depending
    on the context.
    """
    properties: bool = False
    parse_fields: bool = False
    user_fields: bool = False
    metadata_fields: bool = False

    def has(self, cls: Type[AbstractNodeData]) -> bool:
        """
        Return whether this set of field kinds accepts ``cls``.
        """
        return (
            (self.properties and issubclass(cls, PropertyDef))
            or (self.parse_fields and issubclass(cls, Field))
            or (self.user_fields and issubclass(cls, UserField))
            or (self.metadata_fields and issubclass(cls, MetadataField))
        )


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
        return denoted_str(args[0])


class ExternalAnnotationSpec(AnnotationSpec):
    """
    Interpreter for the @external annotation on properties.
    """

    @dataclass
    class Value:
        uses_envs: bool = False
        uses_entity_info: bool = False

    def __init__(self) -> None:
        super().__init__(
            "external", unique=True, require_args=True, default_value=None
        )

    def interpret(
        self,
        ctx: CompileCtx,
        args: List[L.Expr],
        kwargs: Dict[str, L.Expr],
        scope: Scope,
    ) -> Any:
        for arg in args:
            with ctx.lkt_context(arg):
                error("no positional argument expected")

        result = self.Value()
        for k, v in kwargs.items():
            if k == "uses_envs":
                result.uses_envs = parse_static_bool(ctx, v)
            elif k == "uses_entity_info":
                result.uses_entity_info = parse_static_bool(ctx, v)
            else:
                error(f"invalid keyword argument: {k}")
        return result


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
    Interpreter for @unparsing_spacing annotations for token families.
    """
    def __init__(self) -> None:
        super().__init__('unparsing_spacing', unique=False, require_args=True)

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

        # Get the lexer declaration (find_toplevel_decl checks that there is
        # exactly one).
        full_decl = find_toplevel_decl(
            ctx, ctx.lkt_units, L.LexerDecl, "lexer"
        )
        decl = full_decl.f_decl
        assert isinstance(decl, L.LexerDecl)

        # Make sure the name mentionned in this annotation matches the actual
        # lexer name.
        with ctx.lkt_context(requested_name):
            check_source_language(
                decl.f_syn_name.text == requested_name.text,
                f"Invalid lexer name: '{decl.f_syn_name.text}' expected",
            )
            return decl


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
    with_unparsing_newline: bool
    pre_rule: bool
    ignored: bool
    annotations = [TokenAnnotationSpec('text'),
                   TokenAnnotationSpec('trivia'),
                   TokenAnnotationSpec('symbol'),
                   FlagAnnotationSpec('with_unparsing_newline'),
                   FlagAnnotationSpec('pre_rule'),
                   FlagAnnotationSpec('ignored')]


@dataclass
class LexerAnnotations(ParsedAnnotations):
    indentation_tracking: bool
    annotations = [FlagAnnotationSpec('indentation_tracking')]


@dataclass
class TokenFamilyAnnotations(ParsedAnnotations):
    unparsing_spacing: List[L.RefId]
    annotations = [SpacingAnnotationSpec()]


@dataclass
class BaseNodeAnnotations(ParsedAnnotations):
    custom_short_image: bool
    generic_list_type: str | None
    with_abstract_list: bool
    ple_unit_root: bool
    rebindable: bool
    repr_name: str | None
    snaps: bool
    synthetic: bool
    annotations = [
        FlagAnnotationSpec("custom_short_image"),
        StringLiteralAnnotationSpec("generic_list_type"),
        FlagAnnotationSpec("with_abstract_list"),
        FlagAnnotationSpec("ple_unit_root"),
        StringLiteralAnnotationSpec("repr_name"),
        FlagAnnotationSpec("rebindable"),
        FlagAnnotationSpec("snaps"),
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
    exported: bool
    final: bool
    lazy: bool
    null_field: bool
    nullable: bool
    parse_field: bool
    traced: bool
    used_in_equality: bool
    annotations = [FlagAnnotationSpec('abstract'),
                   FlagAnnotationSpec('exported'),
                   FlagAnnotationSpec('final'),
                   FlagAnnotationSpec('lazy'),
                   FlagAnnotationSpec('null_field'),
                   FlagAnnotationSpec('nullable'),
                   FlagAnnotationSpec('parse_field'),
                   FlagAnnotationSpec('traced'),
                   FlagAnnotationSpec('used_in_equality')]


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
    exported: bool
    external: ExternalAnnotationSpec.Value | None
    final: bool
    ignored: bool
    memoized: bool
    predicate_error: str | None
    traced: bool
    with_dynvars: list[tuple[Scope.DynVar, L.Expr | None]] | None
    annotations = [
        FlagAnnotationSpec('abstract'),
        FlagAnnotationSpec('call_memoizable'),
        StringLiteralAnnotationSpec('call_non_memoizable_because'),
        FlagAnnotationSpec('exported'),
        ExternalAnnotationSpec(),
        FlagAnnotationSpec('final'),
        FlagAnnotationSpec('ignored'),
        StringLiteralAnnotationSpec('predicate_error'),
        FlagAnnotationSpec('memoized'),
        FlagAnnotationSpec('traced'),
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


class FunctionParamSpec:
    """
    Specification for a function parameter.
    """

    def __init__(
        self,
        name: str,
        optional: bool = False,
        keyword_only: bool = False,
    ):
        """
        :param optional: Whether passing an argument for this parameter is
            optional.
        """
        self.name = name
        self.optional = optional
        self.keyword_only = keyword_only


AnyFunctionParamSpec = TypeVar("AnyFunctionParamSpec", bound=FunctionParamSpec)


class FunctionSignature(Generic[AnyFunctionParamSpec]):
    """
    Specification of required/accepted parameters for a function.

    ``AnyFunctionParamSpec`` designates whatever metadata is associated to each
    function parameter.
    """

    def __init__(
        self,
        *param_specs: AnyFunctionParamSpec,
        positional_variadic: bool = False,
    ):
        """
        Create a function signature.

        :param param_specs: List of parameter specifications.
        :param positional_variadic: Whether this function accepts an arbitrary
            number of positional arguments in addition to the ones described by
            ``param_specs``.
        """
        self.param_specs = list(param_specs)
        self.positional_variadic = positional_variadic

        self.positionals = [
            spec for spec in self.param_specs if not spec.keyword_only
        ]
        """Subset of parameters that can be passed as positional arguments."""

        self.by_name: dict[str, AnyFunctionParamSpec] = {}
        for spec in self.param_specs:
            assert spec.name not in self.by_name
            self.by_name[spec.name] = spec

    def match(
        self,
        ctx: CompileCtx,
        call: L.CallExpr
    ) -> tuple[dict[str, L.Expr], list[L.Expr]]:
        """
        Match call arguments against this signature. If successful, return the
        parsed arguments. Abort with a user error otherwise.

        The result is a dict that maps parameter names to syntactically passed
        arguments, and a list of remaining arguments if
        ``self.positional_variadic`` is true.
        """
        # Match results
        args: dict[str, L.Expr] = {}
        vargs: list[L.Expr] = []

        # Index in "self.positionals" for the next expected positional
        # argument.
        next_positional = 0

        for arg in call.f_args:
            if arg.f_name:
                # This is a keyword argument. Make sure it is known and that it
                # was not passed already.
                name = arg.f_name.text
                with ctx.lkt_context(arg.f_name):
                    check_source_language(
                        name in self.by_name, "unknown argument"
                    )
                    check_source_language(
                        name not in args, "this argument is already passed"
                    )
                args[name] = arg.f_value

            else:
                # This is a positional argument: look for the corresponding
                # parameter spec. First update "next_positional" in case they
                # were passed by keyword.
                while (
                    next_positional < len(self.positionals)
                    and self.positionals[next_positional].name in args
                ):
                    next_positional += 1

                if next_positional < len(self.positionals):
                    args[self.positionals[next_positional].name] = arg.f_value
                else:
                    with ctx.lkt_context(arg):
                        check_source_language(
                            self.positional_variadic,
                            f"at most {next_positional} positional argument(s)"
                            f" expected, got {next_positional + 1}",
                        )
                    vargs.append(arg.f_value)

        # Check that all required arguments were passed
        missing = (
            {name for name, spec in self.by_name.items() if not spec.optional}
            - set(args)
        )
        if missing:
            loc_node = (
                call.f_name.f_suffix
                if isinstance(call.f_name, L.DotExpr) else
                call
            )
            with ctx.lkt_context(loc_node):
                error(f"argument '{list(missing)[0]}' is missing")

        return args, vargs


add_env_signature = FunctionSignature(
    FunctionParamSpec("no_parent", optional=True, keyword_only=True),
    FunctionParamSpec("transitive_parent", optional=True, keyword_only=True),
    FunctionParamSpec("names", optional=True, keyword_only=True),
)
"""
Signature for the "add_env" env action.
"""

add_to_env_kv_signature = FunctionSignature(
    FunctionParamSpec("key"),
    FunctionParamSpec("value"),
    FunctionParamSpec("dest_env", optional=True, keyword_only=True),
    FunctionParamSpec("metadata", optional=True, keyword_only=True),
    FunctionParamSpec("resolver", optional=True, keyword_only=True),
)
"""
Signature for the "add_to_env_kv" env action.
"""

add_to_env_signature = FunctionSignature(
    FunctionParamSpec("mappings"),
    FunctionParamSpec("resolver", optional=True, keyword_only=True),
)
"""
Signature for the "add_to_env" env action.
"""

append_rebinding_signature = FunctionSignature(
    FunctionParamSpec("old_env"),
    FunctionParamSpec("new_env"),
)
"""
Signature for ".append_rebinding".
"""

collection_iter_signature = FunctionSignature(FunctionParamSpec("expr"))
"""
Common signature for collection iteration expressions.
"""

concat_rebindings_signature = FunctionSignature(
    FunctionParamSpec("rebindings"),
)
"""
Signature for ".concat_rebindings".
"""

contains_signature = FunctionSignature(FunctionParamSpec("value"))
"""
Signature for ".contains".
"""

do_signature = FunctionSignature(
    FunctionParamSpec("expr"),
    FunctionParamSpec("default_val", optional=True, keyword_only=True),
)
"""
Signature for ".do".
"""

do_env_signature = FunctionSignature(FunctionParamSpec("expr"))
"""
Signature for the "do" env action.
"""

domain_signature = FunctionSignature(
    FunctionParamSpec("var"), FunctionParamSpec("domain")
)
"""
Signature for "%domain".
"""

dynamic_lexical_env_signature = FunctionSignature(
    FunctionParamSpec("assocs"),
    FunctionParamSpec("assoc_resolver", optional=True, keyword_only=True),
    FunctionParamSpec("transitive_parent", optional=True, keyword_only=True),
)
"""
Signature for the "dynamic_lexical_env" builtin function.
"""

empty_signature: FunctionSignature[FunctionParamSpec] = FunctionSignature()
"""
Signature for a function that takes no argument.
"""

env_group_signature = FunctionSignature(
    FunctionParamSpec("with_md", optional=True, keyword_only=True),
)
"""
Signature for ".env_group".
"""

eq_signature = FunctionSignature(
    FunctionParamSpec("from"),
    FunctionParamSpec("to"),
    FunctionParamSpec("conv_prop", optional=True, keyword_only=True),
    FunctionParamSpec("logic_ctx", optional=True, keyword_only=True),
)
"""
Signature for "%eq".
"""

filtermap_signature = FunctionSignature(
    FunctionParamSpec("expr"), FunctionParamSpec("filter")
)
"""
Signature for ".[i]filtermap".
"""

get_signature = FunctionSignature(
    FunctionParamSpec("symbol"),
    FunctionParamSpec("lookup", optional=True, keyword_only=True),
    FunctionParamSpec("from", optional=True, keyword_only=True),
    FunctionParamSpec("categories", optional=True, keyword_only=True),
)
"""
Signature for ".get"/".get_first".
"""

is_visible_from_signature = FunctionSignature(FunctionParamSpec("unit"))
"""
Signature for ".is_visible_from".
"""

join_signature = FunctionSignature(FunctionParamSpec("strings"))
"""
Signature for ".join".
"""

logic_all_any_signature = FunctionSignature(FunctionParamSpec("equations"))
"""
Signature for "%all" and for "%any".
"""

predicate_signature = FunctionSignature(
    FunctionParamSpec("pred_prop"),
    FunctionParamSpec("node"),
    FunctionParamSpec("error_location", optional=True, keyword_only=True),
    positional_variadic=True,
)
"""
Signature for "%predicate".
"""

propagate_signature = FunctionSignature(
    FunctionParamSpec("dest"),
    FunctionParamSpec("comb_prop"),
    FunctionParamSpec("logic_ctx", optional=True, keyword_only=True),
    positional_variadic=True,
)
"""
Signature for "%propagate".
"""

rebind_env_signature = FunctionSignature(FunctionParamSpec("env"))
"""
Signature for ".rebind_env".
"""

reference_signature = FunctionSignature(
    FunctionParamSpec("nodes"),
    FunctionParamSpec("resolver"),
    FunctionParamSpec("kind", optional=True, keyword_only=True),
    FunctionParamSpec("dest_env", optional=True, keyword_only=True),
    FunctionParamSpec("cond", optional=True, keyword_only=True),
    FunctionParamSpec("category", optional=True, keyword_only=True),
    FunctionParamSpec(
        "shed_corresponding_rebindings", optional=True, keyword_only=True
    ),
)
"""
Signature for the "reference" env action.
"""

set_initial_env_signature = FunctionSignature(FunctionParamSpec("env"))
"""
Signature for the "set_initial_env" env action.
"""

shed_rebindings_signature = FunctionSignature(FunctionParamSpec("entity_info"))
"""
Signature for ".shed_rebindings".
"""


def create_lexer(ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]) -> Lexer:
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

    @dataclass
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
        token_set: Optional[Set[TokenAction]] = None,
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
        with ctx.lkt_context(r):
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
                denoted_str(decl.f_val), Location.from_lkt_node(decl)
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
                with ctx.lkt_context(child_list):
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
        with ctx.lkt_context(expr):
            if isinstance(expr, L.TokenLit):
                return Literal(denoted_str(expr), location=loc)
            elif isinstance(expr, L.TokenNoCaseLit):
                return NoCaseLit(denoted_str(expr.f_lit), location=loc)
            elif isinstance(expr, L.TokenPatternLit):
                return Pattern(denoted_str(expr), location=loc)
            else:
                error('Invalid lexing expression')

    def lower_token_ref(ref: L.RefId) -> Action:
        """
        Return the Token that `ref` refers to.
        """
        with ctx.lkt_context(ref):
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
        with ctx.lkt_context(full_decl):
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
            with ctx.lkt_context(r):
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
                   lexer_annot.indentation_tracking,
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
    root_scope = create_root_scope(ctx)

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

            all_rules[rule_name] = (r, r.f_expr)

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
                            with ctx.lkt_context(rule.f_sep.f_extra):
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
                    with ctx.lkt_context(prop_ref):
                        error(
                            'reference to a property expected'
                            ' ("Node.property")'
                        )

                # First resolve the reference to the node
                node = resolve_node_ref(prop_ref.f_prefix)

                # Then resolve the reference to the property
                with ctx.lkt_context(prop_ref.f_suffix):
                    prop_name = prop_ref.f_suffix.text
                    try:
                        prop = node.get_abstract_node_data_dict()[prop_name]
                    except KeyError:
                        error(f"{node.dsl_name} has no such entity")

                    if not isinstance(prop, PropertyDef):
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

    for name, (rule_doc, rule_expr) in grammar._all_lkt_rules.items():
        grammar._add_rule(name, lower(rule_expr), ctx.lkt_doc(rule_doc))


# Mapping to associate declarations to the corresponding AbstractVariable
# instances. This is useful when lowering expressions.
LocalsEnv = Dict[L.BaseValDecl, AbstractVariable]


class LktTypesLoader:
    """
    Helper class to instantiate ``CompiledType`` for all types described in
    Lkt.
    """

    # Map Lkt type declarations to the corresponding CompiledType instances, or
    # to None when the type declaration is currently being lowered. Keeping a
    # None entry in this case helps detecting illegal circular type
    # dependencies.
    compiled_types: Dict[L.TypeDecl, Optional[CompiledType]]

    ###################
    # Builtin helpers #
    ###################

    @dataclass
    class Generics:
        """
        Holder for all the built-in Lkt generics.
        """
        ast_list: Scope.Generic
        array: Scope.Generic
        entity: Scope.Generic
        iterator: Scope.Generic
        node: Scope.Generic
        node_builder: Scope.Generic

    @dataclass
    class Functions:
        dynamic_lexical_env: Scope.BuiltinFunction

    #############################
    # Property lowering helpers #
    #############################

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

    @dataclass
    class BuiltinCallInfo:
        """
        Information about the call to a builtin operation that takes a lambda
        as the first argument, plus optional keyword arguments.
        """
        kwargs: dict[str, L.Expr]
        """
        Keyword arguments passed after the lambda expression.
        """

        scope: Scope
        """
        New scope to lower lambda function arguments and inner expression.
        """

        largs: list[L.LambdaArgDecl]
        """
        List of arguments for this lambda expression.
        """

        expr: L.Expr
        """
        Lambda expression "body".
        """

    @dataclass
    class CollectionLoweringResult:
        """
        Container for the result of the "lower_collection_iter" function.
        """

        inner_expr: AbstractExpression
        """
        Expression to evaluate each element of the array the collection
        expression computes.
        """

        lambda_arg_infos: list[E.LambdaArgInfo]
        """
        Information about all lambda arguments involved in this expression.
        """

        element_var: AbstractVariable
        """
        Iteration variable to hold each collection element.
        """

        index_var: AbstractVariable | None
        """
        Iteration variable to hold each collection element index, if needed.
        """

    @dataclass
    class DeclAction:
        """
        Helper for block lowering. Represents a declaration in a block
        expression.
        """

        var: AbstractVariable
        """
        Abstract variable corresponding to an entity declared in a block.
        """

        init_expr: AbstractExpression
        """
        Initialization expression for this variable.
        """

        location: Location
        """
        Location of this variable declaration. This may not be ``var``'s
        location for "declarations" that represent dynamic variable bindings:
        the dynamic variable is declared at the module level, whereas this
        "declaration" is located inside a property ("VarBind" Lkt node).
        """

    def __init__(self, ctx: CompileCtx, lkt_units: List[L.AnalysisUnit]):
        """
        :param ctx: Context in which to create these types.
        :param lkt_units: Non-empty list of analysis units where to look for
            type declarations.
        """
        self.ctx = ctx

        #
        # ROOT_SCOPE_CREATION
        #

        self.root_scope = root_scope = create_root_scope(ctx)

        # Create a special scope to resolve the "kind" argument for
        # "reference()" env actions.
        self.refd_env_scope = Scope("builtin scope", ctx)
        for ref_kind_value in RefKind:
            self.refd_env_scope.mapping[ref_kind_value.name] = (
                Scope.RefKindValue(ref_kind_value.name, ref_kind_value)
            )

        self.compiled_types: dict[L.Decl, CompiledType | None] = {}
        self.internal_property_counter = iter(itertools.count(0))

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
            Scope.Generic("Array"),
            Scope.Generic("Entity"),
            Scope.Generic("Iterator"),
            Scope.Generic("Node"),
            Scope.Generic("NodeBuilder"),
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
                builtin_type("Address"),
                builtin_type("AnalysisUnit"),
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
                builtin_type("LexicalEnv"),
                builtin_type("LogicContext"),
                builtin_type("LogicVar"),
                builtin_type("LookupKind"),
                builtin_type("RefCategories"),
                builtin_type("SolverDiagnostic"),
                builtin_type("SolverResult"),
                builtin_type("SourceLocation"),
                builtin_type("SourceLocationRange"),
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
                self.generics.array,
                self.generics.entity,
                self.generics.iterator,
                self.generics.node,
                self.generics.node_builder,
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
        root_node_decl: L.BasicClassDecl | None = None
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

                    # Keep track of anyhing that looks like the root node
                    if (
                        isinstance(decl, L.BasicClassDecl)
                        and decl.p_base_type is None
                    ):
                        root_node_decl = decl

                elif isinstance(decl, L.DynVarDecl):
                    dyn_vars.append(decl)
                else:
                    error(
                        "invalid top-level declaration:"
                        f" {decl.p_decl_type_name}"
                    )

        # There is little point going further if we have not found the root
        # node type.
        if root_node_decl is None:
            error("no node type declaration found")

        # Register the automatic generic list type
        full_decl = cast(L.FullDecl, root_node_decl.parent)
        annotations = parse_annotations(
            self.ctx, NodeAnnotations, full_decl, self.root_scope
        )
        generic_list_type_name = (
            annotations.generic_list_type
            or f"{root_node_decl.f_syn_name.text}BaseList"
        )
        root_scope.mapping[generic_list_type_name] = Scope.BuiltinType(
            generic_list_type_name,
            TypeRepo.Defer(
                lambda: resolve_type(T.root_node.generic_list_type),
                "generic list type",
            ),
        )

        # Make sure we have a Metadata type registered in the root scope so
        # that Lkt code can refer to it.
        if "Metadata" not in root_scope.mapping:
            self.root_scope.mapping["Metadata"] = Scope.BuiltinType(
                name="Metadata",
                defer=T.deferred_type("Metadata"),
            )

        #
        # DYNVAR_LOWERING
        #

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

        #
        # TYPES_LOWERING
        #

        # Now create CompiledType instances for each user type. To properly
        # handle node derivation, this recurses on bases first and reject
        # inheritance loops.
        self.properties_to_lower: list[LktTypesLoader.PropertyToLower] = []
        self.env_specs_to_lower: list[tuple[ASTNodeType, L.EnvSpecDecl]] = []
        for type_decl in type_decls:
            self.lower_type_decl(type_decl)

        # If user code does not define one, create a default Metadata struct
        # and make it visible in the root scope. Otherwise, validate it.
        if CompiledTypeRepo.env_metadata is None:
            self.ctx.create_default_metadata()
            root_scope.mapping["Metadata"] = builtin_type("Metadata")
        else:
            ctx.check_env_metadata(CompiledTypeRepo.env_metadata)

        # Make sure entity types are created
        entity_info_type = T.entity_info
        assert isinstance(entity_info_type, CompiledType)
        entity_type = T.entity
        assert isinstance(entity_type, CompiledType)

        #
        # TYPES_RESOLUTION
        #

        # Resolve type references in all type fields
        for entity in self.root_scope.mapping.values():
            if isinstance(entity, (Scope.BuiltinType, Scope.UserType)):
                t = resolve_type(entity.defer)
                for field in t._fields.values():
                    field.resolve_types()

        #
        # ENV_SPECS_LOWERING
        #

        # Note that lowering env specs needs to be done before any expression
        # is constructed: the set of legal ref categories is determined by env
        # specs, and constructing RefCategory expressions needs to know the
        # whole set of ref categories.
        for node, env_spec_decl in self.env_specs_to_lower:
            env_spec = self.lower_env_spec(node, env_spec_decl)
            node.env_spec = env_spec
            env_spec.ast_node = node
            env_spec.register_categories(self.ctx)

        #
        # EXPR_LOWERING
        #

        # Now that all user-defined compiled types are known, we can start
        # lowering expressions and env specs. Start with default values for
        # property arguments and dynamic variables.
        for to_lower in self.properties_to_lower:
            for arg_decl, arg in zip(
                to_lower.arguments, to_lower.prop.arguments
            ):
                if arg_decl.f_default_val is not None:
                    value = self.lower_static_expr(
                        arg_decl.f_default_val, self.root_scope
                    )
                    value.prepare()
                    arg.set_default_value(value)

            if to_lower.dynamic_vars is not None:
                to_lower.prop.set_dynamic_vars(
                    [
                        (
                            dynvar,
                            None
                            if init_expr is None else
                            self.lower_static_expr(init_expr, self.root_scope)
                        )
                        for dynvar, init_expr in to_lower.dynamic_vars
                    ]
                )

        # Now that all types and properties ("declarations") are available,
        # lower the property expressions themselves.
        for to_lower in self.properties_to_lower:
            if isinstance(to_lower, self.PropertyAndExprToLower):
                with to_lower.prop.bind():
                    self.reset_names_counter()
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
                error(f"generic expected, got {result.diagnostic_name}")

    def resolve_type(self, name: L.TypeRef, scope: Scope) -> TypeRepo.Defer:
        """
        Like ``resolve_entity``, but for types specifically.
        """
        # A general note regarding validation: translating TypeRef nodes to
        # TypeRepo.Defer instances is done during lowering, and checking
        # whether some type is a node requires lowering. As a consequence,
        # these checks must be deferred in order to avoid chicken and egg
        # problems (type lowering infinite recursion).
        #
        # To implement these deferred checks, we use the
        # TypeRepo.Defer.with_check method to add checks to the process of
        # CompiledType resolution.

        if isinstance(name, L.GenericTypeRef):
            with self.ctx.lkt_context(name):
                generic = self.resolve_generic(name.f_type_name, scope)
                type_args = list(name.f_params)
                if generic == self.generics.ast_list:
                    if len(type_args) != 1:
                        error(
                            f"{generic.name} expects one type argument: the"
                            " list element type"
                        )
                    element_type, = type_args

                    # Check that the element type is a node and that the
                    # designated root node is indeed the root node.
                    return self.resolve_node(element_type, scope).list

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
                    return self.resolve_node(node_type, scope).entity

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

                elif generic == self.generics.node_builder:
                    if len(type_args) != 1:
                        error(
                            f"{generic.name} expects one type argument: the"
                            " node type"
                        )
                    node_type, = type_args
                    return self.resolve_node(node_type, scope).builder_type

                else:
                    # User code cannot define new generics, so there cannot
                    # possibly be other generics.
                    assert False

        elif isinstance(name, L.SimpleTypeRef):
            return self.resolve_type_expr(name.f_type_name, scope)

        else:
            with self.ctx.lkt_context(name):
                error("invalid type reference")

    def type_predicate_check(
        self,
        name: L.TypeRef,
        defer: TypeRepo.Defer,
        predicate: Callable[[CompiledType], bool],
        error_message: str,
    ) -> Callable[[], None]:
        """
        Return a "check" callback that, given a compiled type, emits an error
        if that type does not satisfy a predicate.

        :param name: Lkt node that designates the compiled type to check.
        :param defer: Defer instance for the compiled type to check.
        :param predicate: Compiled type predicate that returns whether the type
            passes the check.
        :param error_message: Message for the error to emit if the type fails
            the check.
        """
        def check() -> None:
            with self.ctx.lkt_context(name):
                check_source_language(predicate(defer.get()), error_message)

        return check

    def root_node_check(
        self,
        name: L.TypeRef,
        scope: Scope,
    ) -> Callable[[], None]:
        """
        Return a check (for `TypeRepo.Defer.with_check`) for the compiled type
        designated by ``name`` in Lkt sources and that ``defer`` resolves to.
        This check ensures that this type is the root node.
        """
        return self.type_predicate_check(
            name,
            self.resolve_type(name, scope),
            lambda t: t.is_root_node, "root node expected",
        )

    def resolve_node(
        self,
        name: L.TypeRef,
        scope: Scope,
    ) -> TypeRepo.Defer:
        """
        Like ``resolve_type``, but checks that the resolved type is a node.
        """
        defer = self.resolve_type(name, scope)
        return defer.with_check(
            self.type_predicate_check(
                name,
                defer,
                lambda t: isinstance(t, ASTNodeType),
                "node expected",
            )
        )

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
                base_type_decl = entity.diagnostic_node
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

                # Lower type arguments
                type_args = [
                    self.resolve_and_lower_node(t, scope)
                    for t in name.f_params
                ]
                check_source_language(
                    len(type_args) == 1,
                    f"{generic.name} expects type argument: the list element"
                    " type"
                )
                return type_args[0].list

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
        allowed_field_kinds: FieldKinds,
        user_field_public: bool,
        struct_name: str,
    ) -> AbstractNodeData:
        """
        Lower the field described in ``decl``.

        :param allowed_field_kinds: Set of field kinds allowed for the fields
            to load.
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
                'public': annotations.exported,
                'return_type': field_type,
                'kind': (AbstractKind.abstract
                         if annotations.abstract
                         else AbstractKind.concrete),
                'activate_tracing': annotations.traced,
            }

        elif annotations.parse_field:
            assert decl.f_default_val is None
            check_source_language(
                not annotations.exported,
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
                not annotations.traced,
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
                not annotations.exported,
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
                not annotations.traced,
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
            # constructor. Reject @used_in_equality annotations otherwise, as
            # they are valid only for metadata fields.
            if allowed_field_kinds.metadata_fields:
                cls = constructor = MetadataField
                kwargs["use_in_equality"] = annotations.used_in_equality
            else:
                check_source_language(
                    not annotations.used_in_equality,
                    "Only metadata fields can have the @used_in_equality"
                    " annotation",
                )

        check_source_language(
            allowed_field_kinds.has(cls), 'Invalid field type in this context'
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

    def lower_static_expr(
        self,
        expr: L.Expr,
        env: Scope,
    ) -> AbstractExpression:
        """
        Lower the given expression, checking that it is a valid compile time
        known value.
        """
        return self.lower_expr(
            expr, env, local_vars=None, static_required=True
        )

    def create_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        lower_expr: Callable[[PropertyDef], AbstractExpression],
        rtype: CompiledType | None,
        location: Location,
    ) -> PropertyDef:
        """
        Create an internal property.

        This is similar to ``lower_expr_to_internal_property``, but with a
        callback to get the lowered expression body.
        """
        result = PropertyDef(
            expr=None,
            prefix=AbstractNodeData.PREFIX_INTERNAL,
            name=names.Name.from_lower(
                f"{name}_{next(self.internal_property_counter)}"
            ),
            public=False,
            type=rtype,
        )

        # Make internal properties unreachable from user code
        result._indexing_name = f"_{result.original_name.lower()}"
        result._original_name = result._indexing_name

        # Internal properties never have dynamic variables
        result.set_dynamic_vars([])

        self.reset_names_counter()

        with result.bind():
            result.expr = lower_expr(result)

        # Register this new property as a field of the owning node
        node.add_field(result)

        result.location = location
        return result

    def reset_names_counter(self) -> None:
        """
        Reset the counter used to generate names that are unique inside a
        property (e.g. for local variables).

        This method must be called each time we are about to lower a property's
        body expression.
        """
        self.names_counter = itertools.count(0)

    @overload
    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        expr: L.Expr | AbstractExpression,
        rtype: CompiledType | None,
    ) -> PropertyDef: ...

    @overload
    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        expr: None,
        rtype: CompiledType | None,
    ) -> None: ...

    def lower_expr_to_internal_property(
        self,
        node: ASTNodeType,
        name: str,
        expr: L.Expr | AbstractExpression | None,
        rtype: CompiledType | None,
    ) -> PropertyDef | None:
        """
        Create an internal property to lower an expression.

        For convenience, accept a null body expression: return None in that
        case (create no property).

        :param node: Node for which we want to create this property.
        :param name: Name prefix, used to generate the actual property name.
        :param expr: Body for this proprety.
        :param rtype: Return type for this property.
        """
        if expr is None:
            return None
        not_none_expr = expr

        def lower_expr(p: PropertyDef) -> AbstractExpression:
            expr = not_none_expr

            # If the body is a Lkt expression, lower it. Use it unchanged
            # otherwise.
            return (
                self.lower_expr(expr, self.root_scope, p.vars)
                if isinstance(expr, L.Expr) else
                expr
            )

        return self.create_internal_property(
            node,
            name,
            lower_expr,
            rtype,
            location=(
                Location.from_lkt_node(expr)
                if isinstance(expr, L.Expr) else
                Location.builtin
            )
        )

    @staticmethod
    def extract_call_args(
        expr: L.CallExpr
    ) -> tuple[list[L.Expr], dict[str, L.Expr]]:
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

    def lower_call_args(
        self,
        expr: L.CallExpr,
        lower: Callable[[L.Expr], AbstractExpression],
    ) -> tuple[list[AbstractExpression], dict[str, AbstractExpression]]:
        """
        Collect call positional and keyword arguments.
        """
        arg_nodes, kwarg_nodes = self.extract_call_args(expr)
        args = [lower(v) for v in arg_nodes]
        kwargs = {k: lower(v) for k, v in kwarg_nodes.items()}
        return args, kwargs

    def lower_method_call(
        self,
        call_expr: L.CallExpr,
        env: Scope,
        local_vars: Optional[LocalVars],
    ) -> AbstractExpression:
        """
        Subroutine for "lower_expr": lower specifically a method call.

        :param call_expr: Method call to lower.
        :param env: Scope to use when resolving references.
        :param local_vars: If lowering a property expression, set of local
            variables for this property.
        """

        result: AbstractExpression

        def lower(expr: L.Expr) -> AbstractExpression:
            """
            Convenience wrapper around "self.lower_expr" to set the expression
            location.
            """
            with AbstractExpression.with_location(
                Location.from_lkt_node(expr)
            ):
                return self.lower_expr(expr, env, local_vars)

        def add_lambda_arg_to_scope(
            scope: Scope,
            arg: L.LambdaArgDecl,
            var: AbstractVariable
        ) -> None:
            """
            Helper to register a lambda expression argument in a scope.
            """
            scope.add(Scope.LocalVariable(arg.f_syn_name.text, arg, var))

        def append_lambda_arg_info(
            infos: list[E.LambdaArgInfo],
            arg: L.LambdaArgDecl,
            var: AbstractVariable,
        ) -> None:
            if arg.f_decl_type is not None:
                infos.append(
                    E.LambdaArgInfo(
                        var,
                        resolve_type(self.resolve_type(arg.f_decl_type, env)),
                        Location.from_lkt_node(arg.f_decl_type),
                    )
                )

        def var_for_lambda_arg(
            scope: Scope,
            arg: L.LambdaArgDecl,
            infos: list[E.LambdaArgInfo],
            prefix: str,
            type: Optional[CompiledType] = None,
            create_local: bool = False,
        ) -> AbstractVariable:
            """
            Create an AbstractVariable to translate a lambda argument.

            This also registers this decl/variable association in ``env``.

            :param scope: Scope in which to register this variable.
            :param arg: Lambda argument to lower.
            :param infos: List of lambda argument information in which to
                append information for this argument, in case the argument
                declaration contains a type annotation.
            :param prefix: Lower-case prefix for the name of the variable in
                the generated code.
            :param type: Optional type information to associate to this
                variable.
            :param create_local: See the corresponding AbstractVariable
                constructor argument.
            """
            source_name, _ = extract_var_name(self.ctx, arg.f_syn_name)
            with AbstractExpression.with_location(Location.from_lkt_node(arg)):
                result = AbstractVariable(
                    names.Name.from_lower(
                        f"{prefix}_{next(self.names_counter)}"
                    ),
                    source_name=source_name,
                    type=type,
                    create_local=create_local,
                )
            add_lambda_arg_to_scope(scope, arg, result)
            append_lambda_arg_info(infos, arg, result)
            return result

        def extract_lambda(
            expr: L.LambdaExpr,
            lambda_n_args: int,
        ) -> tuple[Scope, list[L.LambdaArgDecl], L.Expr]:
            """
            Extract arguments/expr from a lambda expression.

            :param expr: Lambda expression to analyze.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            actual_n_args = len(expr.f_params)
            with self.ctx.lkt_context(expr.f_params):
                check_source_language(
                    actual_n_args == lambda_n_args,
                    f"{lambda_n_args} arguments expected, got {actual_n_args}",
                )
            for larg in expr.f_params:
                with self.ctx.lkt_context(larg):
                    check_source_language(
                        larg.f_default_val is None,
                        "default values are not allowed here",
                    )

            loc = Location.from_lkt_node(expr)
            scope = env.create_child(
                f"scope for lambda expression at {loc.gnu_style_repr()}"
            )

            return (scope, list(expr.f_params), expr.f_body)

        def extract_lambda_and_kwargs(
            expr: L.CallExpr,
            signature: FunctionSignature,
            arg_for_lambda: str,
            lambda_n_args: int,
        ) -> LktTypesLoader.BuiltinCallInfo:
            """
            Extract arguments from a call expression, expecting the first
            positional argument to be a lambda expression.

            :param expr: Call expression that is supposed to pass the lambda
                expression.
            :param signature: Signature for the builtin function that is
                called.
            :param arg_for_lambda: Name of the argument in ``signature`` that
                must contain the lambda.
            :param lambda_n_args: Number of arguments expected for the lambda
                expression.
            """
            # Make sure the only positional argument is a lambda expression
            args, _ = signature.match(self.ctx, expr)
            lambda_expr = args[arg_for_lambda]
            if not isinstance(lambda_expr, L.LambdaExpr):
                with self.ctx.lkt_context(lambda_expr):
                    error("lambda expression expected")

            # Extract info from the lambda expression itself
            scope, lambda_args, lambda_body = extract_lambda(
                lambda_expr, lambda_n_args
            )

            return LktTypesLoader.BuiltinCallInfo(
                args, scope, lambda_args, lambda_body
            )

        def lower_collection_iter(
            has_index: bool,
        ) -> LktTypesLoader.CollectionLoweringResult:
            """
            Helper to lower a method call that implements a collection
            iteration.

            This assumes that that ``call_expr`` is such a method call: the
            signature for this method is ``collection_iter_signature``, and its
            ``expr`` argument is expected to be a lambda function to process
            one collection element. That lambda function must accept the
            collection element itself only (if ``has_index`` is false) or an
            additional element index (if ``has_index`` is true).

            Return the lowered expression for the lambda, information for
            lambda args, the variable for the iteration element, and an
            optional variable for the iteration index.
            """
            # We expect a single argument: a lambda (itself taking the
            # collection element plus optionally its index).
            lambda_info = extract_lambda_and_kwargs(
                call_expr,
                collection_iter_signature,
                "expr",
                2 if has_index else 1
            )
            lambda_arg_infos: list[E.LambdaArgInfo] = []
            element_arg = lambda_info.largs[0]
            if has_index:
                index_arg = lambda_info.largs[1]

            # There is always an iteration variable for the collection element
            element_var = var_for_lambda_arg(
                lambda_info.scope, element_arg, lambda_arg_infos, 'item'
            )

            # The iteration variable for the iteration index is optional: we
            # create one only if the lambda has the corresponding element.
            index_var: AbstractVariable | None = None
            if has_index:
                index_var = var_for_lambda_arg(
                    lambda_info.scope,
                    index_arg,
                    lambda_arg_infos,
                    'index',
                    T.Int,
                )

            # Lower the body expression for that lambda
            inner_expr = self.lower_expr(
                lambda_info.expr, lambda_info.scope, local_vars
            )
            return LktTypesLoader.CollectionLoweringResult(
                inner_expr, lambda_arg_infos, element_var, index_var
            )

        def lower_node_builder(prefix: L.Expr) -> AbstractExpression:
            """
            Helper to lower the creation of a synthetizing node builder.

            :param prefix: Prefix for the ".builder()" method, i.e. the
                expected synthetic node type reference.
            """
            with self.ctx.lkt_context(prefix):
                if not isinstance(prefix, (L.DotExpr, L.TypeRef, L.RefId)):
                    error("Prefix for .builder expressions must be a node")

            node_type = self.resolve_type_expr(prefix, env)

            args, kwargs = self.lower_call_args(call_expr, lower)
            with self.ctx.lkt_context(call_expr.f_args):
                if len(args) != 0:
                    error("Positional arguments not allowed for .builder")

            return E.CreateSynthNodeBuilder(node_type, **kwargs)

        call_name = call_expr.f_name
        assert isinstance(call_name, L.BaseDotExpr)

        method_name = call_name.f_suffix.text

        # Handle node builder creation from node types
        if method_name == "builder":
            return lower_node_builder(call_name.f_prefix)

        # TODO (eng/libadalang/langkit#728): introduce a pre-lowering pass to
        # extract the list of types and their fields/methods so that we can
        # perform validation here.
        method_prefix = lower(call_name.f_prefix)

        # Add the right wrappers to handle null conditional constructs. Note
        # that anything going through "getattr" will take care of validating
        # Check and adding a Prefix wrapper: adjust wrappers accordingly for
        # them.
        getattr_prefix = method_prefix
        if isinstance(call_name, L.NullCondDottedName):
            getattr_prefix = NullCond.Check(getattr_prefix, validated=False)
            method_prefix = NullCond.Check(method_prefix, validated=True)
        method_prefix = NullCond.Prefix(method_prefix)

        # Make sure this is not an attempt to call a builin field
        try:
            BuiltinAttribute[method_name]
        except KeyError:
            pass
        else:
            with self.ctx.lkt_context(call_name.f_suffix):
                error("this is a builtin attribute, it should not be called")

        # Handle calls to builtin methods and regular properties separately
        try:
            builtin = BuiltinMethod[method_name]
        except KeyError:
            call_args, call_kwargs = self.lower_call_args(call_expr, lower)
            return E.FieldAccess(
                method_prefix,
                method_name,
                E.FieldAccess.Arguments(call_args, call_kwargs),
                check_call_syntax=True,
            )

        # Past this point, we know that this is a builtin method call
        if builtin in (
            BuiltinMethod.all,
            BuiltinMethod.any,
            BuiltinMethod.iall,
            BuiltinMethod.iany,
        ):
            clr = lower_collection_iter(
                has_index=builtin in (BuiltinMethod.iall, BuiltinMethod.iany),
            )
            result = E.Quantifier.create_expanded(
                (
                    "all"
                    if builtin in (BuiltinMethod.all, BuiltinMethod.iall) else
                    "any"
                ),
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
            )

        elif builtin == BuiltinMethod.append_rebinding:
            args, _ = append_rebinding_signature.match(self.ctx, call_expr)
            result = getattr_prefix.append_rebinding(
                lower(args["old_env"]), lower(args["new_env"])
            )

        elif builtin == BuiltinMethod.as_array:
            empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.as_array

        elif builtin == BuiltinMethod.as_big_int:
            empty_signature.match(self.ctx, call_expr)
            result = E.BigIntLiteral(method_prefix)

        elif builtin == BuiltinMethod.as_int:
            empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.as_int

        elif builtin == BuiltinMethod.concat_rebindings:
            args, _ = concat_rebindings_signature.match(self.ctx, call_expr)
            result = getattr_prefix.concat_rebindings(
                lower(args["rebindings"])
            )

        elif builtin == BuiltinMethod.contains:
            args, _ = contains_signature.match(self.ctx, call_expr)
            result = getattr_prefix.contains(lower(args["value"]))

        elif builtin == BuiltinMethod.do:
            lambda_info = extract_lambda_and_kwargs(
                call_expr, do_signature, "expr", 1
            )
            arg_node = lambda_info.largs[0]

            lambda_arg_infos: list[E.LambdaArgInfo] = []
            arg_var = var_for_lambda_arg(
                lambda_info.scope,
                arg_node,
                lambda_arg_infos,
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
                lambda_arg_infos,
                arg_var,
                default_val,
            )

        elif builtin == BuiltinMethod.empty:
            empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, "empty")

        elif builtin == BuiltinMethod.env_group:
            args, _ = env_group_signature.match(self.ctx, call_expr)
            with_md_expr = args.get("with_md")
            with_md = None if with_md_expr is None else lower(with_md_expr)
            result = getattr_prefix.env_group(with_md=with_md)

        elif builtin == BuiltinMethod.env_orphan:
            empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.env_orphan

        elif builtin in (BuiltinMethod.filter, BuiltinMethod.ifilter):
            clr = lower_collection_iter(
                has_index=builtin == BuiltinMethod.ifilter
            )
            result = E.Map.create_expanded(
                method_prefix,
                clr.element_var,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                clr.inner_expr,
            )

        elif builtin in (BuiltinMethod.filtermap, BuiltinMethod.ifiltermap):
            has_index = builtin == BuiltinMethod.ifiltermap
            lambda_n_args = 2 if has_index else 1

            # Validate arguments for ".[i]filtermap()" itself
            args, _ = filtermap_signature.match(self.ctx, call_expr)
            for arg in [args["expr"], args["filter"]]:
                if not isinstance(arg, L.LambdaExpr):
                    with self.ctx.lkt_context(arg):
                        error("lambda expressions expceted")

            # Validate and analyze the two lambda expressions
            lambda_0 = args["expr"]
            assert isinstance(lambda_0, L.LambdaExpr)
            map_scope, map_args, map_body = extract_lambda(
                lambda_0, lambda_n_args
            )

            lambda_1 = args["filter"]
            assert isinstance(lambda_1, L.LambdaExpr)
            filter_scope, filter_args, filter_body = extract_lambda(
                lambda_1, lambda_n_args
            )

            # We need to have two different scopes for the two lambda
            # expressions, but need to create common iteration variables for
            # both.
            lambda_arg_infos = []
            element_var = var_for_lambda_arg(
                map_scope,
                map_args[0],
                lambda_arg_infos,
                "item",
            )
            name_from_lower(self.ctx, "argument", filter_args[0].f_syn_name)
            add_lambda_arg_to_scope(filter_scope, filter_args[0], element_var)
            append_lambda_arg_info(
                lambda_arg_infos, filter_args[0], element_var
            )

            index_var: AbstractVariable | None = None
            if has_index:
                index_var = var_for_lambda_arg(
                    map_scope, map_args[1], lambda_arg_infos, "index", T.Int
                )
                name_from_lower(
                    self.ctx, "argument", filter_args[1].f_syn_name
                )
                add_lambda_arg_to_scope(
                    filter_scope, filter_args[1], index_var
                )
                append_lambda_arg_info(
                    lambda_arg_infos, filter_args[1], index_var
                )

            # Lower their expressions
            map_expr = self.lower_expr(map_body, map_scope, local_vars)
            filter_expr = self.lower_expr(
                filter_body, filter_scope, local_vars
            )

            return E.Map.create_expanded(
                collection=method_prefix,
                expr=map_expr,
                lambda_arg_infos=lambda_arg_infos,
                element_var=element_var,
                index_var=index_var,
                filter_expr=filter_expr,
            )

        elif builtin == BuiltinMethod.find:
            lambda_info = extract_lambda_and_kwargs(
                call_expr, collection_iter_signature, "expr", 1
            )
            elt_arg = lambda_info.largs[0]

            lambda_arg_infos = []
            elt_var = var_for_lambda_arg(
                lambda_info.scope,
                elt_arg,
                lambda_arg_infos,
                'item',
            )
            inner_expr = self.lower_expr(
                lambda_info.expr, lambda_info.scope, local_vars
            )

            result = E.Find.create_expanded(
                method_prefix,
                inner_expr,
                lambda_arg_infos,
                elt_var,
                index_var=None,
            )
        elif builtin in (BuiltinMethod.get, BuiltinMethod.get_first):
            args, _ = get_signature.match(self.ctx, call_expr)
            symbol = lower(args["symbol"])

            lookup_expr = args.get("lookup")
            lookup: AbstractExpression | None = (
                None if lookup_expr is None else lower(lookup_expr)
            )

            from_node_expr = args.get("from")
            from_node: AbstractExpression | None = (
                None if from_node_expr is None else lower(from_node_expr)
            )

            categories_expr = args.get("categories")
            categories: AbstractExpression | None = (
                None if categories_expr is None else lower(categories_expr)
            )

            return (
                getattr_prefix.get(symbol, lookup, from_node, categories)
                if method_name == "get" else
                getattr_prefix.get_first(symbol, lookup, from_node, categories)
            )

        elif builtin == BuiltinMethod.get_value:
            empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.get_value

        elif builtin == BuiltinMethod.is_visible_from:
            args, _ = is_visible_from_signature.match(self.ctx, call_expr)
            result = getattr_prefix.is_visible_from(lower(args["unit"]))

        elif builtin == BuiltinMethod.join:
            args, _ = join_signature.match(self.ctx, call_expr)
            result = getattr_prefix.join(lower(args["strings"]))

        elif builtin == BuiltinMethod.length:
            empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, "length")

        elif builtin in (
            BuiltinMethod.ilogic_all,
            BuiltinMethod.ilogic_any,
            BuiltinMethod.logic_all,
            BuiltinMethod.logic_any,
        ):
            import langkit.expressions.logic as LE

            has_index = builtin in (
                BuiltinMethod.ilogic_all, BuiltinMethod.ilogic_any
            )
            is_all = builtin in (
                BuiltinMethod.ilogic_all, BuiltinMethod.logic_all
            )

            clr = lower_collection_iter(has_index=has_index)
            map_expr = E.Map.create_expanded(
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
            )
            result = (
                LE.All(map_expr)
                if is_all else
                LE.Any(map_expr)
            )

        elif builtin in (
            BuiltinMethod.imap,
            BuiltinMethod.imapcat,
            BuiltinMethod.map,
            BuiltinMethod.mapcat,
        ):
            clr = lower_collection_iter(
                has_index=builtin in (
                    BuiltinMethod.imap, BuiltinMethod.imapcat
                )
            )
            result = E.Map.create_expanded(
                method_prefix,
                clr.inner_expr,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                do_concat=builtin in (
                    BuiltinMethod.mapcat, BuiltinMethod.imapcat
                ),
            )

        elif builtin == BuiltinMethod.rebind_env:
            args, _ = rebind_env_signature.match(self.ctx, call_expr)
            result = getattr_prefix.rebind_env(lower(args["env"]))

        elif builtin == BuiltinMethod.singleton:
            empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, "singleton")

        elif builtin == BuiltinMethod.shed_rebindings:
            args, _ = shed_rebindings_signature.match(self.ctx, call_expr)
            result = getattr_prefix.shed_rebindings(lower(args["entity_info"]))

        elif builtin in (
            BuiltinMethod.solve, BuiltinMethod.solve_with_diagnostics
        ):
            empty_signature.match(self.ctx, call_expr)
            result = getattr(getattr_prefix, method_name)

        elif builtin == BuiltinMethod.super:
            call_args, call_kwargs = self.lower_call_args(call_expr, lower)
            result = E.Super(method_prefix, *call_args, **call_kwargs)

        elif builtin in (
            BuiltinMethod.itake_while, BuiltinMethod.take_while
        ):
            clr = lower_collection_iter(
                has_index=builtin == BuiltinMethod.itake_while
            )
            result = E.Map.create_expanded(
                method_prefix,
                clr.element_var,
                clr.lambda_arg_infos,
                clr.element_var,
                clr.index_var,
                take_while_expr=clr.inner_expr,
            )

        elif builtin == BuiltinMethod.to_builder:
            empty_signature.match(self.ctx, call_expr)
            result = E.CreateCopyNodeBuilder(method_prefix)

        elif builtin == BuiltinMethod.unique:
            empty_signature.match(self.ctx, call_expr)
            result = getattr_prefix.unique

        elif builtin == BuiltinMethod.update:
            arg_nodes, kwarg_nodes = self.extract_call_args(call_expr)
            if arg_nodes:
                with self.ctx.lkt_context(arg_nodes[0]):
                    error(".update() accepts keyword arguments only")
            field_exprs = {k: lower(v) for k, v in kwarg_nodes.items()}
            result = E.StructUpdate(method_prefix, **field_exprs)

        else:
            assert False, f"unhandled builitn call: {call_name.f_suffix}"

        return result

    def lower_expr(self,
                   expr: L.Expr,
                   env: Scope,
                   local_vars: Optional[LocalVars],
                   static_required: bool = False) -> AbstractExpression:
        """
        Lower the given expression.

        :param expr: Expression to lower.
        :param env: Scope to use when resolving references.
        :param local_vars: If lowering a property expression, set of local
            variables for this property.
        :param static_required: Whether "expr" is required to be a static
            expression.
        """

        def abort_if_static_required(expr: L.Expr) -> None:
            """
            Abort lowering if a static expression is required for "expr".
            """
            if static_required:
                with self.ctx.lkt_context(expr):
                    error("static expression expected in this context")

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

            if isinstance(expr, L.AnyOf):
                abort_if_static_required(expr)

                prefix = lower(expr.f_expr)
                return E.AnyOf(
                    lower(expr.f_expr),
                    *[lower(v) for v in expr.f_values],
                )

            elif isinstance(expr, L.ArrayLiteral):
                abort_if_static_required(expr)

                elts = [lower(e) for e in expr.f_exprs]
                element_type = (
                    None
                    if expr.f_element_type is None else
                    self.resolve_type(expr.f_element_type, env)
                )
                return E.ArrayLiteral(elts, element_type=element_type)

            elif isinstance(expr, L.BigNumLit):
                abort_if_static_required(expr)

                text = expr.text
                assert text[-1] == 'b'
                return E.BigIntLiteral(int(text[:-1]))

            elif isinstance(expr, L.BinOp):
                abort_if_static_required(expr)

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

                elif isinstance(expr.f_op, L.OpOrInt):
                    return left._or(right)

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
                abort_if_static_required(expr)

                assert local_vars is not None
                loc = Location.from_lkt_node(expr)
                sub_env = env.create_child(
                    f"scope for block at {loc.gnu_style_repr()}"
                )

                actions: list[LktTypesLoader.DeclAction] = []

                for v in expr.f_val_defs:
                    var: AbstractVariable
                    init_abstract_expr: L.Expr
                    scope_var: Scope.UserValue

                    if isinstance(v, L.ValDecl):
                        # Create the AbstractVariable for this declaration
                        source_name = v.f_syn_name.text
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
                        scope_var = Scope.LocalVariable(source_name, v, var)

                    elif isinstance(v, L.VarBind):
                        # Look for the corresponding dynamic variable, either
                        # unbound (DynVar, that we will bound) or already
                        # bounded (BoundDynVar, that we will rebind in this
                        # scope).
                        entity = self.resolve_entity(v.f_name, sub_env)
                        if not isinstance(
                            entity, (Scope.DynVar, Scope.BoundDynVar)
                        ):
                            with self.ctx.lkt_context(v.f_name):
                                error(
                                    "dynamic variable expected, got"
                                    f" {entity.diagnostic_name}"
                                )

                        var = entity.variable
                        init_abstract_expr = v.f_expr
                        scope_var = Scope.BoundDynVar(v.f_name.text, v, var)

                    else:
                        assert False, f'Unhandled def in BlockExpr: {v}'

                    # Lower the declaration/bind initialization expression
                    init_expr = self.lower_expr(
                        init_abstract_expr, sub_env, local_vars
                    )

                    # Make the declared value/dynamic variable available to the
                    # remaining expressions.
                    sub_env.add(scope_var)
                    actions.append(
                        LktTypesLoader.DeclAction(
                            var, init_expr, Location.from_lkt_node(v)
                        )
                    )

                # Lower the block main expression and wrap it in declarative
                # blocks.
                result = self.lower_expr(expr.f_expr, sub_env, local_vars)
                for action in reversed(actions):
                    with AbstractExpression.with_location(action.location):
                        if isinstance(action.var, E.DynamicVariable):
                            result = getattr(action.var, "bind")(
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

                def lower_new(type_ref: TypeRepo.Defer) -> AbstractExpression:
                    """
                    Consider that this call creates a new struct, return the
                    corresponding New expression.
                    """
                    # Non-struct/node types have their own constructor
                    if type_ref.get() == T.RefCategories:
                        arg_nodes, kwarg_nodes = self.extract_call_args(
                            call_expr
                        )
                        check_source_language(
                            len(arg_nodes) == 0,
                            "Positional arguments not allowed for"
                            " RefCategories",
                        )

                        default_expr = kwarg_nodes.pop("_", None)
                        enabled_categories = {
                            k: parse_static_bool(self.ctx, v)
                            for k, v in kwarg_nodes.items()
                        }
                        return E.RefCategories(
                            default=(
                                False
                                if default_expr is None else
                                parse_static_bool(self.ctx, default_expr)
                            ),
                            **enabled_categories,
                        )
                    else:
                        abort_if_static_required(expr)

                        args, kwargs = self.lower_call_args(call_expr, lower)
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
                        abort_if_static_required(expr)

                        args, _ = dynamic_lexical_env_signature.match(
                            self.ctx, call_expr
                        )
                        trans_parent_expr = args.get("transitive_parent")
                        return E.DynamicLexicalEnv(
                            assocs_getter=lower(args["assocs"]),
                            assoc_resolver=self.resolve_property(
                                args.get("assoc_resolver")
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
                    abort_if_static_required(expr)

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

                    node_arg = type_args[0]
                    node_ref = self.resolve_type(node_arg, env)
                    with self.ctx.lkt_context(node_arg):
                        node_type = resolve_type(node_ref)
                        check_source_language(
                            isinstance(node_type, ASTNodeType),
                            "Node type expected"
                        )
                    return lower_new(node_ref.entity)

                # Otherwise the call has to be a dot expression, for a method
                # invocation.
                elif not isinstance(call_name, L.BaseDotExpr):
                    with self.ctx.lkt_context(call_name):
                        error("invalid call prefix")

                abort_if_static_required(expr)

                return self.lower_method_call(call_expr, env, local_vars)

            elif isinstance(expr, L.CastExpr):
                abort_if_static_required(expr)

                subexpr = lower(expr.f_expr)
                excludes_null = expr.f_excludes_null.p_as_bool
                dest_type = self.resolve_type(expr.f_dest_type, env)
                return Cast(subexpr, dest_type, do_raise=excludes_null)

            elif isinstance(expr, L.CharLit):
                return E.CharacterLiteral(denoted_char(expr))

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
                abort_if_static_required(expr)

                prefix = lower(expr.f_prefix)
                suffix = expr.f_suffix.text

                # Make sure this is not an attempt to access a builtin method
                try:
                    BuiltinMethod[suffix]
                except KeyError:
                    pass
                else:
                    with self.ctx.lkt_context(expr.f_suffix):
                        error("this is a builtin method, it should be called")

                # Handle accesses to builtin attributes and regular field
                # access separately.
                #
                # In both cases, add the NullCond.Check wrapper when needed. In
                # the case of builtin attributes, getattr will take care of
                # introducing the NullCheck.Prefix wrapper and validating the
                # .Check one.
                try:
                    BuiltinAttribute[suffix]
                except KeyError:
                    if null_cond:
                        prefix = NullCond.Check(prefix, validated=True)
                    return E.FieldAccess(
                        NullCond.Prefix(prefix),
                        suffix,
                        check_call_syntax=True,
                    )
                else:
                    if null_cond:
                        prefix = NullCond.Check(prefix, validated=False)
                    return getattr(prefix, suffix)

            elif isinstance(expr, L.IfExpr):
                abort_if_static_required(expr)

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
                abort_if_static_required(expr)

                subexpr = lower(expr.f_expr)
                nodes = [
                    self.resolve_type(type_ref, env)
                    for type_ref in expr.f_dest_type
                ]
                return E.IsA(subexpr, *nodes)

            elif isinstance(expr, L.LogicExpr):
                abort_if_static_required(expr)

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
                        args, _ = logic_all_any_signature.match(self.ctx, expr)
                        eq_array_expr = lower(args["equations"])
                        return (
                            ELogic.All(eq_array_expr)
                            if call_name.text == "all" else
                            ELogic.Any(eq_array_expr)
                        )

                    elif call_name.text == "domain":
                        args, _ = domain_signature.match(self.ctx, expr)
                        logic_var = lower(args["var"])
                        domain_expr = lower(args["domain"])
                        return logic_var.domain(domain_expr)

                    elif call_name.text == "eq":
                        args, _ = eq_signature.match(self.ctx, expr)
                        logic_ctx_expr = args.get("logic_ctx")
                        return E.Bind(
                            lower(args["from"]),
                            lower(args["to"]),
                            conv_prop=self.resolve_property(
                                args.get("conv_prop")
                            ),
                            logic_ctx=(
                                None
                                if logic_ctx_expr is None else
                                lower(logic_ctx_expr)
                            ),
                        )

                    elif call_name.text == "predicate":
                        args, vargs = predicate_signature.match(self.ctx, expr)
                        pred_prop = self.resolve_property(args["pred_prop"])
                        node_expr = lower(args["node"])
                        error_location_expr = args.get("error_location")
                        arg_exprs = [lower(arg) for arg in vargs]
                        return E.Predicate(
                            pred_prop,
                            node_expr,
                            error_location=(
                                None
                                if error_location_expr is None else
                                lower(error_location_expr)
                            ),
                            *arg_exprs,
                        )

                    elif call_name.text == "propagate":
                        args, vargs = propagate_signature.match(self.ctx, expr)
                        dest_var = lower(args["dest"])
                        comb_prop = self.resolve_property(args["comb_prop"])
                        logic_ctx_expr = args.get("logic_ctx")
                        arg_vars = [lower(arg) for arg in vargs]
                        return E.NPropagate(
                            dest_var,
                            comb_prop,
                            logic_ctx=(
                                None
                                if logic_ctx_expr is None else
                                lower(logic_ctx_expr)
                            ),
                            *arg_vars,
                        )

                with self.ctx.lkt_context(expr):
                    error("invalid logic expression")

            elif isinstance(expr, L.KeepExpr):
                abort_if_static_required(expr)

                subexpr = lower(expr.f_expr)
                keep_type = self.resolve_type(expr.f_keep_type, env)
                return subexpr.filtermap(
                    lambda e: e.cast(keep_type),
                    lambda e: e.is_a(keep_type),
                )

            elif isinstance(expr, L.MatchExpr):
                abort_if_static_required(expr)
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
                    match_var.create_local_variable()

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
                abort_if_static_required(expr)

                return E.Not(lower(expr.f_expr))

            elif isinstance(expr, L.NullLit):
                result_type = self.resolve_type(expr.f_dest_type, env)
                return E.No(result_type)

            elif isinstance(expr, L.NumLit):
                return E.Literal(int(expr.text))

            elif isinstance(expr, L.ParenExpr):
                return E.Paren(lower(expr.f_expr))

            elif isinstance(expr, L.RaiseExpr):
                abort_if_static_required(expr)

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
                args_nodes, kwargs_nodes = self.extract_call_args(cons_expr)
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
                    msg = denoted_str(msg_expr)

                return entity.constructor(
                    self.resolve_type(expr.f_dest_type, env), msg
                )

            elif isinstance(expr, L.RefId):
                entity = self.resolve_entity(expr, env)
                if isinstance(entity, Scope.BuiltinValue):
                    if not isinstance(entity.value, E.Literal):
                        abort_if_static_required(expr)

                    return entity.value
                elif isinstance(entity, Scope.UserValue):
                    abort_if_static_required(expr)
                    return E.VariableRef(entity.variable)
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
                abort_if_static_required(expr)

                string_prefix = expr.p_prefix
                string_value = denoted_str(expr)
                if string_prefix == "\x00":
                    return E.String(string_value)
                elif string_prefix == "s":
                    return E.SymbolLiteral(string_value)
                else:
                    error("invalid string prefix")

            elif isinstance(expr, L.SubscriptExpr):
                abort_if_static_required(expr)

                null_cond = isinstance(expr, L.NullCondSubscriptExpr)
                prefix = lower(expr.f_prefix)
                index = lower(expr.f_index)
                return (
                    prefix.at(index)
                    if isinstance(expr, L.NullCondSubscriptExpr) else
                    prefix.at_or_raise(index)
                )

            elif isinstance(expr, L.TryExpr):
                abort_if_static_required(expr)

                return E.Try(
                    try_expr=lower(expr.f_try_expr),
                    else_expr=(
                        None
                        if expr.f_or_expr is None
                        else lower(expr.f_or_expr)
                    ),
                )

            elif isinstance(expr, L.UnOp):
                assert isinstance(expr.f_op, L.OpMinus)
                return E.UnaryNeg(lower(expr.f_expr))

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

        external = False
        uses_entity_info: bool | None = None
        uses_envs: bool | None = None
        if annotations.external is not None:
            external = True
            uses_entity_info = annotations.external.uses_entity_info
            uses_envs = annotations.external.uses_envs

        # Create the property to return
        result = PropertyDef(
            expr=None,
            prefix=AbstractNodeData.PREFIX_PROPERTY,
            doc=self.ctx.lkt_doc(decl),

            # When the @export annotation is missing, use "None" to mean
            # "public status unspecified", as the property can still be public
            # thanks to inheritance.
            public=annotations.exported or None,

            abstract=annotations.abstract,
            type=return_type,
            abstract_runtime_check=False,
            memoized=annotations.memoized,
            call_memoizable=annotations.call_memoizable,
            memoize_in_populate=False,
            external=external,
            uses_entity_info=uses_entity_info,
            uses_envs=uses_envs,
            optional_entity_info=False,

            # When the @ignored annotation is missing, use "None" to mean
            # "same as from base node".
            warn_on_unused=not annotations.ignored and None,

            call_non_memoizable_because=(
                annotations.call_non_memoizable_because
            ),
            activate_tracing=annotations.traced,
            dump_ir=False,
            lazy_field=False,
            final=annotations.final,
            predicate_error=annotations.predicate_error,
        )
        result._doc_location = Location.from_lkt_node_or_none(full_decl.f_doc)

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
                    Scope.BoundDynVar(
                        dynvar.name, dynvar.diagnostic_node, dynvar.variable
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

    def lower_env_spec(
        self,
        node: ASTNodeType,
        env_spec: L.EnvSpecDecl,
    ) -> EnvSpec:
        """
        Lower an env spec for a node.

        :param node: Node for which we want to lower the env spec.
        :param env_spec: Env spec to lower.
        """
        actions = []

        for syn_action in env_spec.f_actions:
            location = Location.from_lkt_node(syn_action)
            assert isinstance(syn_action.f_name, L.RefId)
            action_kind = syn_action.f_name.text
            action: EnvAction
            if action_kind == "add_env":
                args, _ = add_env_signature.match(self.ctx, syn_action)
                action = AddEnv(
                    no_parent=(
                        parse_static_bool(self.ctx, args["no_parent"])
                        if "no_parent" in args else
                        False
                    ),
                    transitive_parent=self.lower_expr_to_internal_property(
                        node,
                        "env_trans_parent",
                        args.get(
                            "transitive_parent"
                        ) or unsugar(False),
                        T.Bool,
                    ),
                    names=self.lower_expr_to_internal_property(
                        node,
                        "env_names",
                        args.get("names"),
                        T.Symbol.array,
                    ),
                    location=location,
                )

            elif action_kind == "add_to_env_kv":
                args, _ = add_to_env_kv_signature.match(self.ctx, syn_action)

                def lower_expr(
                    p: PropertyDef,
                    e: L.Expr,
                ) -> AbstractExpression:
                    """
                    Shortcut for ``self.lower_expr``.
                    """
                    return self.lower_expr(e, self.root_scope, p.vars)

                def lower_prop_expr(p: PropertyDef) -> AbstractExpression:
                    """
                    Lower the body expression of the "mappings" internal
                    property.
                    """
                    return E.New(
                        T.env_assoc,
                        key=lower_expr(p, args["key"]),
                        value=lower_expr(p, args["value"]),
                        dest_env=(
                            lower_expr(p, args["dest_env"])
                            if "dest_env" in args else
                            E.current_env()
                        ),
                        metadata=(
                            lower_expr(p, args["metadata"])
                            if "metadata" in args else
                            E.No(T.env_md)
                        ),
                    )

                action = AddToEnv(
                    mappings=self.create_internal_property(
                        node=node,
                        name="env_mappings",
                        lower_expr=lower_prop_expr,
                        rtype=T.EnvAssoc,
                        location=Location.from_lkt_node(syn_action),
                    ),
                    resolver=self.resolve_property(args.get("resolver")),
                    location=location,
                )

            elif action_kind == "add_to_env":
                args, _ = add_to_env_signature.match(self.ctx, syn_action)

                action = AddToEnv(
                    mappings=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_mappings",
                        expr=args["mappings"],
                        rtype=None,
                    ),
                    resolver=self.resolve_property(args.get("resolver")),
                    location=location,
                )

            elif action_kind == "do":
                args, _ = do_env_signature.match(self.ctx, syn_action)
                action = Do(
                    expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_do",
                        expr=args["expr"],
                        rtype=None,
                    ),
                    location=location,
                )

            elif action_kind == "handle_children":
                args, _ = empty_signature.match(self.ctx, syn_action)
                action = HandleChildren(location=location)

            elif action_kind == "reference":
                args, _ = reference_signature.match(self.ctx, syn_action)

                kind_expr = args.get("kind")
                category_expr = args.get("category")
                shed_rebindings_expr = args.get(
                    "shed_corresponding_rebindings"
                )

                kind = RefKind.normal
                if kind_expr is not None:
                    kind_entity = self.resolve_entity(
                        kind_expr, self.refd_env_scope
                    )
                    assert isinstance(kind_entity, Scope.RefKindValue)
                    kind = kind_entity.value

                shed_rebindings = False
                if shed_rebindings_expr is not None:
                    shed_rebindings = parse_static_bool(
                        self.ctx, shed_rebindings_expr
                    )

                category: str | None = None
                if category_expr is not None:
                    category = parse_static_str(self.ctx, category_expr)

                action = RefEnvs(
                    resolver=self.resolve_property(args["resolver"]),
                    nodes_expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="ref_env_nodes",
                        expr=args["nodes"],
                        rtype=T.root_node.array,
                    ),
                    kind=kind,
                    dest_env=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_dest",
                        expr=args.get("dest_env"),
                        rtype=T.LexicalEnv,
                    ),
                    cond=self.lower_expr_to_internal_property(
                        node=node,
                        name="ref_cond",
                        expr=args.get("cond"),
                        rtype=T.Bool,
                    ),
                    category=category,
                    shed_rebindings=shed_rebindings,
                    location=location,
                )

            elif action_kind == "set_initial_env":
                args, _ = set_initial_env_signature.match(
                    self.ctx, syn_action
                )
                action = SetInitialEnv(
                    env_expr=self.lower_expr_to_internal_property(
                        node=node,
                        name="env_init",
                        expr=args["env"],
                        rtype=T.DesignatedEnv,
                    ),
                    location=location,
                )

            else:
                with self.ctx.lkt_context(syn_action.f_name):
                    error("invalid env action name")
            actions.append(action)

        with self.ctx.lkt_context(env_spec):
            result = EnvSpec(*actions)
        result.location = Location.from_lkt_node(env_spec)
        result.properties_created = True
        return result

    def _lower_fields(
        self,
        decls: L.DeclBlock,
        allowed_field_kinds: FieldKinds,
        user_field_public: bool,
        struct_name: str,
        allow_env_spec: bool = False,
    ) -> tuple[
        List[tuple[names.Name, AbstractNodeData]], L.EnvSpecDecl | None
    ]:
        """
        Lower the fields described in the given DeclBlock node.

        :param decls: Declarations to process.
        :param allowed_field_kinds: Set of field kinds allowed for the fields
            to load.
        :param user_field_public: Whether user fields should be made public.
        :param allow_env_spec: Whether to allow the presence of env spec
            declarations.
        """
        fields = []
        env_spec: L.EnvSpecDecl | None = None
        for full_decl in decls:
            with self.ctx.lkt_context(full_decl):
                decl = full_decl.f_decl
                name = name_from_lower(self.ctx, "field", decl.f_syn_name)

                # If this is actually an env spec, run the dedicated lowering
                # code.
                if isinstance(decl, L.EnvSpecDecl):
                    check_source_language(
                        allow_env_spec,
                        "env specs are allowed in nodes only",
                    )
                    check_source_language(
                        env_spec is None,
                        "only one env_spec block allowed per type",
                    )
                    env_spec = decl
                    continue

                # Otherwise, this is a field or a property
                field: AbstractNodeData
                if isinstance(decl, L.FunDecl):
                    check_source_language(
                        allowed_field_kinds.properties,
                        'Properties not allowed in this context'
                    )
                    field = self.lower_property(full_decl, struct_name)
                else:
                    field = self.lower_base_field(
                        full_decl,
                        allowed_field_kinds,
                        user_field_public,
                        struct_name,
                    )

                field.location = Location.from_lkt_node(decl)
                fields.append((name, cast(AbstractNodeData, field)))

        return fields, env_spec

    def lower_fields_and_env_spec(
        self,
        decls: L.DeclBlock,
        allowed_field_kinds: FieldKinds,
        user_field_public: bool,
        struct_name: str,
    ) -> tuple[
        List[tuple[names.Name, AbstractNodeData]], L.EnvSpecDecl | None
    ]:
        return self._lower_fields(
            decls,
            allowed_field_kinds,
            user_field_public,
            struct_name,
            allow_env_spec=True,
        )

    def lower_fields(
        self,
        decls: L.DeclBlock,
        allowed_field_kinds: FieldKinds,
        user_field_public: bool,
        struct_name: str,
    ) -> List[tuple[names.Name, AbstractNodeData]]:
        fields, _ = self._lower_fields(
            decls, allowed_field_kinds, user_field_public, struct_name
        )
        return fields

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
        allowed_field_kinds = (
            FieldKinds(properties=True, user_fields=True)
            if is_token_node or is_enum_node
            else FieldKinds(
                properties=True,
                parse_fields=True,
                user_fields=True,
            )
        )
        fields, env_spec = self.lower_fields_and_env_spec(
            decl.f_decls,
            allowed_field_kinds,
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
            annotations=langkit.dsl.Annotations(
                repr_name=annotations.repr_name,
                generic_list_type=annotations.generic_list_type,
                rebindable=annotations.rebindable,
                custom_short_image=annotations.custom_short_image,
                snaps=annotations.snaps,
                ple_unit_root=annotations.ple_unit_root,
            ),
            is_abstract=is_abstract,
            is_token_node=is_token_node,
            is_error_node=is_error_node,
            is_synthetic=is_synthetic,
            with_abstract_list=annotations.with_abstract_list,
            is_enum_node=is_enum_node,
            is_bool_node=is_bool_node,
        )
        assert isinstance(decl.parent, L.FullDecl)
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
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

        if env_spec is not None:
            self.env_specs_to_lower.append((result, env_spec))

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
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
        )
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

        allowed_field_kinds: FieldKinds
        if annotations.metadata:
            allowed_field_kinds = FieldKinds(metadata_fields=True)
            check_source_language(
                CompiledTypeRepo.env_metadata is None,
                "Only one struct can be the env metadata",
            )
        else:
            allowed_field_kinds = FieldKinds(user_fields=True)

        fields = self.lower_fields(
            decl.f_decls,
            allowed_field_kinds,
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
        result._doc_location = Location.from_lkt_node_or_none(
            decl.parent.f_doc
        )
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
