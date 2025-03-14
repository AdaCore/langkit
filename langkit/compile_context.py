"""
This file contains the logic for the compilation context for Langkit. This is
the main hook into Langkit, insofar as this is the gate through which an
external language creator will actually trigger the code emission. For example,
this is the way it is done for the Ada language::

    from ada_parser import ada_lexer, ada_grammar
    context = CompileCtx(... ada_lexer, ada_grammar...)
    ...
    context.emit(...)
"""
from __future__ import annotations

from collections import defaultdict
from contextlib import contextmanager
import dataclasses
from enum import Enum
from functools import reduce
import itertools
import os
from os import path
from typing import Any, Callable, Iterable, Sequence, TYPE_CHECKING

import docutils.parsers.rst.roles
from funcy import lzip

from langkit import documentation, names, utils
from langkit.ada_api import AdaAPISettings
from langkit.c_api import CAPISettings
from langkit.config import CompilationConfig, LibraryEntity
from langkit.coverage import GNATcov
from langkit.diagnostics import (
    Location, Severity, WarningSet, check_source_language, diagnostic_context,
    error, non_blocking_error
)
from langkit.documentation import (
    DocDatabase,
    PassthroughNode,
    RstCommentChecker,
)
from langkit.utils import (
    Language,
    LanguageSourcePostProcessors,
    PluginLoader,
    PluginLoadingError,
    SourcePostProcessor,
    TopologicalSortError,
    collapse_concrete_nodes,
    memoized,
    memoized_with_default,
    topological_sort,
)
from langkit.utils.deferred import DeferredEntityResolver


if TYPE_CHECKING:
    from langkit.compiled_types import (
        ASTNodeType,
        AbstractNodeData,
        ArrayType,
        BaseStructType,
        CompiledType,
        EntityType,
        EnumType,
        Field,
        IteratorType,
        NodeBuilderType,
        StructType,
        UserField,
    )
    from langkit.emitter import Emitter
    from langkit.expressions import DynamicVariable, PropertyDef
    from langkit.generic_interface import (
        GenericInterface, InterfaceMethodProfile
    )
    from langkit.lexer import Lexer
    from langkit.lexer.regexp import NFAState
    from langkit.frontend.resolver import Resolver
    from langkit.frontend.types import LktTypesLoader
    from langkit.ocaml_api import OCamlAPISettings
    from langkit.passes import AbstractPass
    from langkit.parsers import GeneratedParser, Grammar, Parser, VarDef
    from langkit.python_api import PythonAPISettings
    from langkit.java_api import JavaAPISettings

    import liblktlang as L


compile_ctx: CompileCtx | None = None


def get_context_or_none() -> CompileCtx | None:
    return compile_ctx


def get_context(or_none: bool = False) -> CompileCtx:
    """
    Return the current compilation context. Meant to be used by the rest of
    Langkit, in any code that has been called as part of the CompileCtx.emit
    primitive.

    :param or_none: If True, return None when there is no context.  Otherwise,
        raise an assertion error when there is no context.
    """
    assert compile_ctx is not None, (
        'Get context has been called in a state in which the compile context'
        ' is not set'
    )
    return compile_ctx


@contextmanager
def global_context(ctx):
    """
    Context manager that temporarily make "ctx" global.

    :param CompileContext ctx: Context to make global.
    """
    global compile_ctx
    old_ctx = compile_ctx
    compile_ctx = ctx
    yield
    compile_ctx = old_ctx


class AdaSourceKind(Enum):
    spec = "spec"
    body = "body"


class Verbosity:
    """
    Helper object to handle verbosity level of notifications during code
    generation.
    """

    NONE = 0
    INFO = 1
    DEBUG = 2

    NAMES = ('none', 'info', 'debug')

    def __init__(self, level):
        """
        Create a verbosity level holder.

        :param level: Verbosity level. Can be either the lower-case name for
            this level or the corresponding integer constant.
        :type level: str|int
        """
        if isinstance(level, str):
            if level not in self.NAMES:
                raise ValueError('Invalid verbosity level: {}'.format(level))
            self.level = self._get(level)
        else:
            if level not in [self._get(name) for name in self.NAMES]:
                raise ValueError('Invalid verbosity level: {}'.format(level))
            self.level = level

    @classmethod
    def _get(cls, name):
        """
        Return the integer constant corresponding to the lower-case "name"
        verbosity level.

        :param str name: Verbosity level name.
        :rtype: int
        """
        return getattr(cls, name.upper())

    def __eq__(self, other):
        return isinstance(other, Verbosity) and self.level == other.level

    def __getattr__(self, name):
        """
        Assuming "name" is a lower-case verbosity level name, return whether
        this instance has a level that is either equal or above it.

        :param str name: Lower-case verbosity level name to compare.
        :rtype: bool
        """
        if name in self.NAMES:
            return self.level >= self._get(name)
        else:
            raise AttributeError()

    def __str__(self):
        for name in self.NAMES:
            if self.level == self._get(name):
                return name
        assert False

    def __repr__(self):
        return str(self)

    @classmethod
    def choices(cls):
        """
        Return a list of instances for all available verbosity levels.

        :rtype: list[Verbosity]
        """
        return [
            cls(getattr(cls, name.upper()))
            for name in cls.NAMES
        ]


class GeneratedException:
    """
    Describe an exception in generated libraries.
    """

    def __init__(self,
                 doc_section: str,
                 package: list[str],
                 name: names.Name,
                 generate_renaming: bool = True):
        """
        :param doc_section: Section in the documentation where this exception
            occurs.
        :param package: Ada package in which this exception is originally
            defined.
        :param name: Name for this exception.
        :param generate_renaming: Whether to generate a renaming for this
            exception in the $.Common generated package.
        """
        self.doc_section = doc_section
        self.package = package
        self.name = name
        self.generate_renaming = generate_renaming

    @property
    def doc_entity(self) -> str:
        """
        Name of the documentation entry for this exception.

        :rtype: str
        """
        return '{}.{}'.format(self.doc_section, self.name.lower)

    @property
    def qualname(self) -> str:
        """
        Fully qualified name to the exception declaration (in Ada).

        :rtype: str
        """
        return '{}.{}'.format('.'.join(self.package), self.name)

    @property
    def kind_name(self) -> names.Name:
        """
        Return the enumeration name corresponding to an exception.

        :rtype: names.Name
        """
        return names.Name('Exception') + self.name


@dataclasses.dataclass(frozen=True)
class DeferredEntities:
    """
    Holder for all ``DeferredEntityResolver`` instances.
    """

    @staticmethod
    def _apply_dynamic_variable_type(
        dynvar: DynamicVariable,
        type: CompiledType,
    ) -> None:
        """
        Assign the given type to the given dynamic variable.
        """
        dynvar.type = type

    dynamic_variable_types: DeferredEntityResolver = dataclasses.field(
        default_factory=lambda: DeferredEntityResolver(
            DeferredEntities._apply_dynamic_variable_type
        )
    )

    type_members: DeferredEntityResolver = dataclasses.field(
        default_factory=lambda: DeferredEntityResolver(lambda t, members: None)
    )

    @staticmethod
    def _apply_implemented_interfaces(
        t: BaseStructType,
        interfaces: list[GenericInterface],
    ) -> None:
        """
        Add the given generic interfaces as interfaces implemented by ``t``.
        """
        t.implements.extend(interfaces)

    implemented_interfaces: DeferredEntityResolver = dataclasses.field(
        default_factory=lambda: DeferredEntityResolver(
            DeferredEntities._apply_implemented_interfaces
        )
    )

    @staticmethod
    def _apply_implemented_method(
        member: AbstractNodeData,
        method: InterfaceMethodProfile,
    ) -> None:
        """
        Mark ``method`` as implemented by ``member``.
        """
        member.implements = method

    implemented_methods: DeferredEntityResolver = dataclasses.field(
        default_factory=lambda: DeferredEntityResolver(
            DeferredEntities._apply_implemented_method
        )
    )


class CompileCtx:
    """State holder for native code emission."""

    c_api_settings: CAPISettings
    python_api_settings: PythonAPISettings
    ocaml_api_settings: OCamlAPISettings
    java_api_settings: JavaAPISettings

    all_passes: list[AbstractPass]
    """
    List of all passes in the Langkit compilation pipeline.
    """

    check_only: bool
    """
    Whether this context is configured to only run checks on the language spec.
    """

    init_hooks: list[Callable[[CompileCtx], None]] = []
    """
    List of callbacks to invoke at the end of the next ``CompileCtx``
    instantiation.
    """

    env_metadata: StructType
    """
    The StructType instance that will be used as the lexical environment
    metadata type.
    """

    root_node_type: ASTNodeType
    """
    The ``ASTNodeType`` instance used as the root type. Every other instance
    must be a descendent of it.
    """

    node_types: list[ASTNodeType]
    """
    List for all ``ASTNodeType`` instances, sorted so that A is before B when A
    is a parent class for B. This sorting is important to output declarations
    in dependency order.

    This is computed in the ``compute_type`` pass.
    """

    _composite_types: list[CompiledType]
    """
    Dependency-sorted list of array and struct types.
    """

    def __init__(
        self,
        config: CompilationConfig,
        plugin_loader: PluginLoader,
        verbosity: Verbosity = Verbosity('none'),
    ):
        """Create a new context for code emission.

        :param config: Configuration for the language to compile.
        :param verbosity: Amount of messages to display on standard output.
            None by default.
        """
        from langkit.python_api import PythonAPISettings
        from langkit.ocaml_api import OCamlAPISettings
        from langkit.java_api import JavaAPISettings
        from langkit.passes import AbstractPass
        from langkit.unparsers import Unparsers

        self.plugin_loader = plugin_loader
        self.config = config

        # Make sure all paths mentionned in the configuration are resolved to
        # absolute paths. This is a no-op if these paths were already resolved.
        self.config.resolve_paths(".")

        self.plugin_passes = AbstractPass.load_plugin_passes(
            plugin_loader, config.plugin_passes
        )
        self.source_post_processors = self.load_source_post_processors(
            plugin_loader, config.emission.source_post_processors
        )

        self.ada_api_settings = AdaAPISettings(self)
        self.c_api_settings = CAPISettings(self)
        self.c_api_settings.lib_name = self.lib_name.lower

        self.verbosity = verbosity

        self.compiled = False
        """
        Whether the language specification was compiled. This is used to avoid
        doing it multiple times.

        :type: bool
        """

        self.lkt_units: list[L.AnalysisUnit] = []
        self.lkt_resolver: Resolver

        self.lexer: Lexer
        self.grammar: Grammar
        self.types_resolver: LktTypesLoader

        self.python_api_settings = PythonAPISettings(self, self.c_api_settings)

        self.ocaml_api_settings = OCamlAPISettings(self, self.c_api_settings)

        self.java_api_settings = JavaAPISettings(self, self.c_api_settings)

        self.fns: set[Parser] = set()
        """
        Set of names (names.Name instances) for all generated parser
        functions. This is used to avoid generating these multiple times.
        """

        self.deferred = DeferredEntities()

        self._interfaces: dict[str, GenericInterface] = {}
        """
        Mapping of all generic interfaces. Keys are camel-case interfaces
        names.
        """

        self._enum_types: list[EnumType] = []
        """
        List of all enumeration types.
        """

        self.synthetic_nodes: list[ASTNodeType] | None = None
        """
        Sub-sequence of `self.node_types` for all nodes that are synthetic.

        This is computed right after `self.node_types`.
        """

        self.node_kind_constants: dict[ASTNodeType, int] = {}
        """
        Mapping: ASTNodeType concrete (i.e. non abstract) instance -> int,
        associating specific constants to be used reliably in bindings.  This
        mapping is built at the beginning of code emission.
        """

        self.kind_constant_to_node: dict[int, ASTNodeType] = {}
        """
        Reverse mapping for `node_kind_constants`.
        """

        self.pending_node_types: list[ASTNodeType] | None = []
        """
        List of node types created before the ``compute_types`` pass. After
        this pass has run, set to None to prevent further types creation.
        """

        self.pending_enum_types: list[EnumType] | None = []
        """
        List of enum types created before the ``compute_types`` pass. After
        this pass has run, set to None to prevent further types creation.
        """

        self.pending_composite_types: list[CompiledType] | None = []
        """
        List of composite types (structs, node builders, arrays, iterators)
        created before the ``compute_composite_types`` pass. After this pass
        has run, set to None to prevent further types creation.
        """

        self._struct_types: list[StructType] | None = None
        """
        List of all plain struct types.
        """

        self._entity_types: list[EntityType] | None = None
        """
        List of all entity types.
        """

        self.generic_list_type: ASTNodeType
        """
        The root gammar class subclass that is the base class for all
        automatically generated root list types.
        """

        self.has_env_metadata = False
        """
        Whether ``self.env_metadata`` was initialized.
        """

        self.has_root_node_type = False
        """
        Whether ``self.root_node_type`` was initialized.
        """

        self.list_types: set[ASTNodeType] = set()
        """
        Set of all ASTNodeType instances for which we generate a corresponding
        list type.
        """

        self.exception_types: dict[str, GeneratedException] = {}
        """
        Mapping of all exception types. Keys are lower-case exception names.
        """

        self.node_builder_types: list[NodeBuilderType]
        """
        List of needed node builder types. Computed during the
        "compute_composite_types" pass.
        """

        self._array_types: list[ArrayType] | None = None
        """
        Sorted list of all ArrayType instances.

        For each ArrayType instance T, code emission for type definition will
        automatically happen.
        """

        self._iterator_types: list[IteratorType] | None = None
        """
        List of all IteratorType instances.
        """

        self.memoized_properties: set[PropertyDef] = set()
        """
        Set of all PropertyDef instances that are memoized.
        """

        self.memoization_keys: set[CompiledType] = set()
        """
        Set of all CompiledType instances that are used as key in the hashed
        maps used to implement properties memoization. All of them must be
        hashable.
        """

        self.memoization_values: set[CompiledType] = set()
        """
        Set of all CompiledType instances that are used as value in the hashed
        maps used to implement properties memoization. Any type can fit, there
        is no restriction.
        """

        self.symbol_literals: dict[str, names.Name] = {}
        """
        Container for all symbol literals to be used in code generation.

        All "built-in" symbol literals used in parsers and properties are
        pre-computed in each analysis context so that parsing and properties
        evaluation does not need to perform symbol table lookups for them.

        Set of such pre-computed symbols is stored in the generated library in
        an array indexed by an enumeration type. Here, this holds a mapping:
        symbol text -> enumerator, to be used for code generation. See the
        "Symbol_Literals" array in $.Analysis.Analysis_Context_Type.

        This mapping is not supposed to be directly modified anywhere else than
        inside this CompileCtx class. See the add_symbol_literal method to add
        symbols to this mapping. Note that this mapping will be empty until one
        calls the finalize_symbol_literals method.
        """

        self._symbol_literals: set[str] = set()
        """
        Temporary container for all symbol literal candidates. This is used
        during the collect "pass" for all symbols. When the set is finalized,
        call the finalize_symbol_literals method to create the
        "symbol_literals" mapping.

        This two-pass mechanism is here to make sure we generate deterministic
        enumeration names.
        """

        #
        # Holders for the Ada generated code chunks
        #

        self.generated_parsers: list[GeneratedParser] = []

        self._extensions_dir = os.path.join(
            self.config.library.root_directory, "extensions"
        )
        """
        Internal field for extensions directory.
        """

        self.has_env_assoc = False
        """
        Whether the env_assoc structure type is created.
        """

        self.has_env_assoc_array = False
        """
        Whether the array type for the env_assoc structure type is created.
        """

        self.has_ref_env = False
        """
        Whether there is a RefEnvs action in environment specs.
        """

        self.additional_source_files: list[str] = []
        """
        List of path for file names to include in the generated library.
        """

        self.logic_functor_props: set[tuple[PropertyDef, int]] = set()
        """
        Set of properties (and the corresponding arity for entity args) used as
        converters/combiners in logic equations. We generate functors for them,
        so that equations can refer to them.
        """

        docs = {
            **documentation.base_langkit_docs,
            **config.extra_docs
        }
        self.documentations: DocDatabase = (
            documentation.instantiate_templates(docs)
        )
        """
        Documentation database. Associate a Mako template for each entity to
        document in the generated library.
        """

        self.parsers_varcontext_stack: list[list[VarDef]] = []
        """
        Holder for the stack of variables contexts used in parsers code
        emission.
        """

        self.warnings: WarningSet = WarningSet()
        """
        Set of warnings to emit.
        """

        for name, enable in config.warnings.items():
            try:
                warning = self.warnings.lookup(name)
            except ValueError as exc:
                with diagnostic_context(Location.nowhere):
                    error(str(exc))
            if enable:
                self.warnings.enable(warning)
            else:
                self.warnings.disable(warning)

        self.with_clauses: dict[
            tuple[str, AdaSourceKind],
            list[tuple[str, bool, bool]]
        ] = defaultdict(list)
        """
        Mapping that binds a list of additional WITH/USE clauses to generate
        for each source file in the generated library. Used to add WITH/USE
        clauses required by extensions. See the `add_with_clause` method.
        """

        self.sorted_public_structs: list[StructType] | None = None
        """
        Sorted list of all public structs. Used to generate the introspection
        API.

        Note that this excludes the entity type: the fact that entities are
        structs is an implementation detail, not exposed to public APIs.
        """

        self.sorted_struct_fields: list[UserField] | None = None
        """
        Sorted list of all public fields for structs in
        ``self.sorted_public_structs``. Used to generate the introspection API.
        """

        self.sorted_parse_fields: list[Field] | None = None
        """
        Sorted list of all parsing fields, minus fields that override abstract
        ones. Used to generate the AST node introspection API.
        """

        self.sorted_properties: list[PropertyDef] | None = None
        """
        Sorted list of public properties. Used to generate the property
        introspection API.
        """

        self.ple_unit_root: ASTNodeType | None = None
        """
        Node to be used as the PLE unit root, if any.
        """

        self.ref_cats = {names.Name.from_lower('nocat')}
        """
        Set of all env lookup categories, used to optionally discriminate
        referenced envs during env lookup.
        """

        self.nfa_start: NFAState | None = None
        """
        Intermediate representation for the lexer state machine (NFA).
        """

        self.unparsers: Unparsers = Unparsers(self)
        """
        :type: langkit.unparsers.Unparsers
        """

        self.lkt_types_loader: LktTypesLoader
        """
        LktTypesLoader singleton. Available only once types lowering from Lkt
        has completed.
        """

        self.emitter: Emitter | None = None
        """
        During code emission, corresponding instance of Emitter. None the rest
        of the time.
        """

        self.gnatcov: GNATcov | None = None
        """
        During code emission, GNATcov instance if coverage is enabled. None
        otherwise.

        :type: None|langkit.coverage.GNATcov
        """

        # Register builtin exception types
        self._register_builtin_exception_types()

        # Register external exceptions
        for ext_exc in config.library.external_exceptions:
            self.register_exception_type(
                package=ext_exc.exception.unit_fqn.split("."),
                name=names.Name(ext_exc.exception.entity_name),
                doc_section=ext_exc.doc_section,
            )

        # Register extra use clauses from the configuration
        for unit, unit_clauses in config.library.extra_context_clauses.items():
            for kind, clauses in [
                (AdaSourceKind.spec, unit_clauses.spec),
                (AdaSourceKind.body, unit_clauses.body),
            ]:
                for c in clauses:
                    self.add_with_clause(
                        unit, kind, c.with_unit, c.use, c.private
                    )

        self.properties_forwards_callgraph: \
            dict[PropertyDef, set[PropertyDef]] | None = None
        """
        Mapping from caller properties to sets of called properties. None when
        not yet computed or invalidated.
        """

        self.properties_backwards_callgraph: \
            dict[PropertyDef, set[PropertyDef]] | None = None
        """
        Mapping from called properties to sets of caller properties. None when
        not yet computed or invalidated.
        """

        self.property_exceptions: list[str] = sorted(
            set(config.library.property_exceptions) | {"Property_Error"}
        )

        self.property_exception_matcher = " | ".join(self.property_exceptions)
        """
        Helper to generate Ada exception handlers to catch errors that
        properties are allowed to raise.
        """

        # Register requested RST passthrough roles
        for name in config.emission.rst_passthrough_roles:
            docutils.parsers.rst.roles.register_local_role(
                name, PassthroughNode.role_fn
            )

        # Run initialization hooks and clear them for the next context
        for func in self.init_hooks:
            func(self)
        self.init_hooks.clear()

        self.internal_members_counter = itertools.count(1)
        """
        Generator used to create unique names for internal members: see
        ``langkit.compiled_types.MemberNames``.
        """

    @property
    def lib_name(self) -> names.Name:
        """
        Shortcut for ``config.library.actual_library_name``.
        """
        return self.config.library.actual_library_name

    @property
    def short_name_or_long(self) -> str:
        """
        Return the library short name (if one is given) or the regular name (if
        there is no short name).
        """
        return self.config.library.short_name or self.lib_name.lower

    @property
    def case_insensitive(self) -> bool:
        """
        Whether the language is supposed to be case insensitive.
        """
        return self.lexer.case_insensitive

    @property
    def generate_unparsers(self) -> bool:
        """
        Whether to include tree unparsing support in the generated library.
        """
        return self.grammar.with_unparsers

    @property
    def symbol_canonicalizer(self) -> LibraryEntity | None:
        if self.config.library.symbol_canonicalizer:
            return self.config.library.symbol_canonicalizer
        elif self.case_insensitive:
            return LibraryEntity("Langkit_Support.Symbols", "Fold_Case")
        else:
            return None

    @staticmethod
    def load_source_post_processors(
        plugin_loader: PluginLoader,
        refs: dict[str, str],
    ) -> LanguageSourcePostProcessors:
        """
        Load the given set of source post processors.
        """
        result: dict[Language, SourcePostProcessor] = {}
        with diagnostic_context(Location.nowhere):
            for lang_name, ref in refs.items():
                try:
                    lang = Language(lang_name)
                except ValueError:
                    error(f"No such language: {lang_name!r}")

                try:
                    # See the comment above PluginLoader.load
                    pp = plugin_loader.load(
                        ref, SourcePostProcessor  # type: ignore
                    )
                except PluginLoadingError as exc:
                    error(str(exc))

                result[lang] = pp
        return result

    def register_exception_type(self,
                                package: list[str],
                                name: names.Name,
                                doc_section: str,
                                is_builtin: bool = False):
        """
        Register an Ada exception that generated bindings may have to translate
        across the language boundaries.

        :param package: Ada package in which this exception is defined.
        :param name: Name of this exception.
        :param doc_section: Name of the section where to document this
            exception.
        :param is_builtin: Whether this is a Langkit built-in exception.
        """
        exception_name = name.lower
        assert exception_name not in self.exception_types

        # Generate in $.Common a renaming for all builtin exceptions. Also,
        # precisely because they are not renamed in $.Common, we must WITH the
        # defining package in $.Implementation.C so that the C API can handle
        # these exceptions.
        generate_renaming = is_builtin
        if not is_builtin:
            self.add_with_clause(
                "Implementation.C",
                AdaSourceKind.body,
                ".".join(package)
            )

        self.exception_types[exception_name] = GeneratedException(
            doc_section, package, name, generate_renaming
        )

    def _register_builtin_exception_types(self):
        """
        Register exception types for all builtin exceptions.
        """
        for namespace, exception_name in [
            (None, 'native_exception'),
            (None, 'precondition_failure'),
            (None, 'property_error'),
            (None, 'invalid_unit_name_error'),
            (None, 'invalid_symbol_error'),
            (None, 'stale_reference_error'),
            (None, 'unknown_charset'),
            (None, 'invalid_input'),
            (None, 'syntax_error'),
            (None, 'file_read_error'),
            ('Introspection', 'bad_type_error'),
            ('Introspection', 'out_of_bounds_error'),
            ('Unparsing', 'malformed_tree_error'),
            ('Rewriting', 'template_format_error'),
            ('Rewriting', 'template_args_error'),
            ('Rewriting', 'template_instantiation_error')
        ]:
            doc_section = 'langkit'
            package = ['Langkit_Support', 'Errors']
            if namespace:
                doc_section = f"{doc_section}.{namespace.lower()}"
                package.append(namespace)

            self.register_exception_type(
                package,
                names.Name.from_lower(exception_name),
                doc_section,
                is_builtin=True
            )

        # Make original exception declarations available to exceptions handlers
        # in the C API.
        self.add_with_clause(
            "Implementation.C", AdaSourceKind.body, "Langkit_Support.Errors"
        )

    def resolve_interface(self, name: str) -> GenericInterface:
        """
        Return the interface with the given name formatted in camel case.
        """
        return self._interfaces[name]

    def resolve_interface_method_qualname(
        self,
        qualname: str,
    ) -> InterfaceMethodProfile:
        """
        Return the interface with the corresponding fully qualified name.
        """
        assert qualname.count(".") == 1
        interface, method = qualname.split(".")
        resolved_interface = self.resolve_interface(interface)
        return resolved_interface.get_method(method)

    @property
    def exceptions_by_section(self) -> list[tuple[str | None,
                                                  list[GeneratedException]]]:
        """
        Return exceptions grouped by "section".

        We compute sections from documentation entries: none for
        'langkit.EXCEPTION_NAME' and SECTION for
        'langkit.SECTION.EXCEPTION_NAME'.

        Note that this skips exceptions for which we don't generate a renaming
        in the generated $.Common spec.
        """
        sections = defaultdict(list)

        for e in self.sorted_exception_types:
            if not e.generate_renaming:
                continue

            # Remove the 'langkit.' prefix
            no_prefix = e.doc_entity.split('.', 1)[1]

            section_name = (
                '' if '.' not in no_prefix else
                no_prefix.split('.')[0].replace('_', ' ').capitalize())

            sections[section_name].append(e)

        return sorted(sections.items())

    def add_with_clause(self, from_pkg, source_kind, to_pkg, use_clause=False,
                        is_private=False):
        """
        Add a WITH clause for `to_pkg` in the `source_kind` part of the
        `from_pkg` generated package.

        :param str from_pkg: Package to which the WITH clause must be added.
        :param AdaSourceKind source_kind: Kind of source file in which the WITH
            clause must be added.
        :param str to_pkg: Name of the Ada package to WITH.
        :param bool use_clause: Whether to generate the corresponding USE
            clause.
        :param bool is_private: Whether to generate a "private with" clause.
        """
        assert not use_clause or not is_private, (
            'Cannot generate a private with clause and a use clause for {}'
            ' (from {}:{})'
            .format(to_pkg, source_kind, from_pkg))
        self.with_clauses[(from_pkg, source_kind)].append(
            (to_pkg, use_clause, is_private))

    def sorted_types(self, type_set):
        """
        Turn "type_set" into a list of types sorted by name.

        This is useful during code generation as sorted types keep a consistent
        order for declarations.

        :param set[langkit.compiled_types.CompiledType] type_set: Set of
            CompiledType instances to sort.
        :rtype: list[langkit.compiled_types.CompiledType]
        """
        return sorted(type_set, key=lambda cls: cls.name)

    @property
    def sorted_exception_types(self) -> list[GeneratedException]:
        """
        Turn "exception_types" into a sorted list.

        This is required during code generation to preserve a stable output.
        """
        return sorted(self.exception_types.values(),
                      key=lambda e: e.doc_entity)

    @staticmethod
    def grammar_rule_api_name(rule):
        """
        Return the API name of the given grammar rule name.

        :type rule: str
        :rtype: names.Name
        """
        return names.Name.from_lower(rule + '_rule')

    @property
    def main_rule_api_name(self):
        """
        Return the API name of the grammar's main rule.

        :rtype: names.Name
        """
        return self.grammar_rule_api_name(self.grammar.main_rule_name)

    def compute_types(self):
        """
        Compute various information related to compiled types, that needs to be
        available for code generation.
        """
        from langkit.compiled_types import EnumType, T

        entity = self.root_node_type.entity

        # Add the root_node_interface in the implemented root node interfaces
        self.deferred.implemented_interfaces.add(
            T.root_node, lambda: [self.resolve_interface("NodeInterface")]
        )

        self.generic_list_type = self.root_node_type.generic_list_type

        # The Group lexical environment operation takes an array of lexical
        # envs, so we always need to generate the corresponding array type.
        _ = T.LexicalEnv.array

        # Likewise for the entity array type (LexicalEnv.get returns it) and
        # for the root node array type (some primitives need that).
        _ = entity.array
        _ = self.root_node_type.array

        # Freeze the set of node types
        node_types = self.pending_node_types
        self.pending_node_types = None

        # Sort them in dependency order as required but also then in
        # alphabetical order so that generated declarations are kept in a
        # relatively stable order. This is really useful for debugging
        # purposes.
        def node_sorting_key(n: ASTNodeType) -> str:
            return n.hierarchical_name

        node_types.sort(key=node_sorting_key)
        self.node_types = node_types

        # Also sort ASTNodeType.subclasses lists
        for n in self.node_types:
            n.subclasses.sort(key=node_sorting_key)

        self.synthetic_nodes = [n for n in self.node_types if n.synthetic]

        # We need a hash function for the metadata structure as the
        # Langkit_Support.Lexical_Env generic package requires it.
        T.env_md.require_hash_function()

        # We expose a hash function for public entities, so we must generate
        # the underlying required helpers.
        T.entity.require_hash_function()

        # Create the type for grammar rules
        EnumType(
            self,
            name='GrammarRule',
            location=None,
            doc="Gramar rule to use for parsing.",
            value_names=[
                self.grammar_rule_api_name(n)
                for n in self.grammar.user_defined_rules
            ],
        )

        # Force the creation of several types, as they are used in templated
        # code.

        # The env assoc types are required by Lexical_Env instantiation and
        # always-emitted PLE helpers.
        _ = T.InnerEnvAssoc.array

        # Arrays of symbols are required to deal with environment names
        T.Symbol.array

        # At this point, all enum types should be known: create the frozen
        # list of enum types.
        assert self.pending_enum_types is not None
        self._enum_types = sorted(
            self.pending_enum_types, key=lambda et: et.name
        )
        self.pending_enum_types = None

    def compute_field_nullability(self) -> None:
        """
        Compute the definite "nullable" information for all fields.

        This pass uses @nullable annotations from the language spec as well as
        information found in the grammar to determine if fields are nullable.

        Note that struct fields (``UserField`` instances) cannot have the
        ``@nullable`` annotation and are always considered nullable.
        """
        from langkit.compiled_types import ASTNodeType, Field
        from langkit.parsers import (Defer, DontSkip, List, Null, Opt, Or,
                                     Parser, Predicate, Skip, StopCut,
                                     _Extract, _Transform)

        all_parse_fields = [
            field
            for node_type in self.node_types
            for field in node_type.get_parse_fields(include_inherited=False)
        ]

        # Only root fields can be annotated with @nullable: overridings just
        # inherit that property.
        for field in all_parse_fields:
            if field.base:
                with field.diagnostic_context:
                    check_source_language(
                        field.nullable_from_spec in (None, False),
                        "Only root fields can be annotated with @nullable",
                        severity=Severity.non_blocking_error,
                    )
                field.nullable_from_spec = field.base.nullable_from_spec

        # Infer field nullability from null fields and from the grammar. If a
        # field is nullable keep track of why (parser can produce null or
        # related field is a null one).
        @dataclasses.dataclass
        class Nullability:
            nullable: bool
            reason: Parser | Field | None

            def set_nullable(self, reason: Parser | Field) -> None:
                self.nullable = True
                self.reason = reason

        # When a field is nullable, its whole field tree (ancestors and all of
        # their overridings) is nullable: create only one Nullability instance
        # for each tree.
        inferred_nullability: dict[Field, Nullability] = {
            field: Nullability(False, None)
            for field in all_parse_fields
            if field.base is None
        }
        for field in all_parse_fields:
            if field.base:
                inferred_nullability[field] = inferred_nullability[field.root]

        # Null fields make their field tree nullable
        for field in all_parse_fields:
            if field.null:
                inferred_nullability[field].set_nullable(field)

        # Now infer nullability from the grammar itself

        @memoized_with_default(False)
        def can_produce_null(parser: Parser) -> bool:
            """
            Return whether ``parser`` can return a null node without a parsing
            error.
            """
            assert isinstance(parser.type, ASTNodeType)

            # Parsers for list types never return null nodes: they create empty
            # list nodes instead.
            if parser.type.is_list_type:
                return False

            if isinstance(parser, Opt):
                # If parser is an Opt parser and is not set to produce an enum
                # alternative, it means that field is optional.
                return not parser._booleanize
            elif isinstance(parser, Null):
                # Null parsers can produce a non-null node for bool nodes
                return not parser.for_bool_node
            elif isinstance(parser, Or):
                return any(can_produce_null(p) for p in parser.parsers)
            elif isinstance(parser, (Defer, StopCut)):
                return can_produce_null(parser.parser)
            elif isinstance(parser, List):
                return False
            elif isinstance(parser, DontSkip):
                return can_produce_null(parser.subparser)
            elif isinstance(parser, Skip):
                return False
            elif isinstance(parser, Predicate):
                return can_produce_null(parser.parser)
            elif isinstance(parser, _Transform):
                return False
            elif isinstance(parser, _Extract):
                return can_produce_null(parser.parser.parsers[parser.index])
            else:
                raise NotImplementedError("Unhandled parser {}".format(parser))

        for field in all_parse_fields:
            n = inferred_nullability[field]
            if not n.nullable:
                for parser in field.parsers_from_transform:
                    if can_produce_null(parser):
                        n.set_nullable(parser)
                        break

        # Set definitive nullability information in fields.
        #
        # Also ensure that all fields declared as *not* nullable in the
        # language spec are not inferred as nullable.
        for field, n in inferred_nullability.items():
            field._nullable = bool(n.nullable or field.nullable_from_spec)

            # Since nullability is inferred and specified for whole field
            # trees, perform validation only on root fields: it is redundant
            # for overriding fields.
            if field.is_overriding:
                continue

            if n.nullable and field.nullable_from_spec is False:
                if isinstance(n.reason, Parser):
                    non_blocking_error(
                        "This parsing rule can assign a null value to"
                        f" {field.qualname}: a @nullable annotation is"
                        " required for that field",
                        location=n.reason.location,
                    )
                elif isinstance(n.reason, Field):
                    assert n.reason.null
                    assert field.location
                    non_blocking_error(
                        "@nullable annotation required because"
                        f" {n.reason.qualname} overrides this field",
                        location=field.location,
                    )
                else:
                    # All nullable fields should have a reason to be this way
                    assert False

            # Also warn about @nullable fields that are never null according to
            # the grammar and never initialized by node synthetization in
            # properties.
            with field.diagnostic_context:
                check_source_language(
                    n.nullable
                    or field._synthetized
                    or not field.nullable_from_spec,
                    "Spurious @nullable annotation: this field is never null"
                    " in the absence of parsing error, and is never"
                    " initialized by node synthetization, so it can never be"
                    " null in practice.",
                    severity=Severity.warning,
                )

    def check_ple_unit_root(self):
        """
        Check that if the "ple_unit_root" node annotation is used, it is valid.
        """
        # Locate the PLE_unit root (if any), checking that we at most one such
        # node annotation.
        for n in self.node_types:
            if not n.annotations.ple_unit_root:
                continue

            with n.diagnostic_context:
                if self.ple_unit_root:
                    check_source_language(
                        False, 'Only one PLE unit root is allowed: {}'
                               .format(self.ple_unit_root.dsl_name)
                    )
                check_source_language(
                    not n.subclasses,
                    'No node can derive from PLE unit roots: here we have'
                    ' {}'.format(', '.join(c.dsl_name for c in n.subclasses))
                )
                check_source_language(
                    not n.synthetic,
                    'Synthetic nodes cannot be PLE unit roots'
                )
                self.ple_unit_root = n

        if self.ple_unit_root is None:
            return

        with diagnostic_context(Location.nowhere):
            check_source_language(
                self.ple_unit_root in self.list_types,
                'At least one parser must create lists of PLE unit roots'
            )
        ple_unit_root_list = self.ple_unit_root.list

        # Check that there is no subclass for lists of PLE unit roots
        for subcls in ple_unit_root_list.subclasses:
            with subcls.diagnostic_context:
                check_source_language(False, 'Lists of PLE unit roots'
                                             ' cannot be subclassed')

        # Finally, check that the only way to get a PLE unit root is as a child
        # of a list node that is itself the root of a tree.
        for n in self.node_types:
            for f in n.get_parse_fields():
                with f.diagnostic_context:
                    check_source_language(
                        ple_unit_root_list not in f.precise_types,
                        '{} cannot appear anywhere in trees except as a root'
                        ' node'.format(ple_unit_root_list.dsl_name)
                    )
                    check_source_language(
                        self.ple_unit_root not in f.precise_types,
                        '{} cannot appear anywhere in trees except as a child'
                        ' of {} nodes'.format(self.ple_unit_root.dsl_name,
                                              ple_unit_root_list.dsl_name)
                    )

    def check_concrete_subclasses(self, astnode):
        """
        Emit an error if `astnode` is abstract and has no concrete subclass.

        :param ASTNodeType astnode: AST node to check.
        """
        # It's fine to have no list type, so as a special case we allow the
        # generic list type to have no concrete subclass.
        if astnode.is_generic_list_type or not astnode.abstract:
            return

        check_source_language(
            astnode.concrete_subclasses,
            '{} is abstract and has no concrete subclass'.format(
                astnode.dsl_name
            )
        )

    def all_properties(self, *args, **kwargs):
        """
        Return an iterator on all the properties. *args and **kwargs are
        forwarded to the call to get_properties that is done on every astnode
        type.

        :rtype: seq[PropertyDef]
        """
        from langkit.compiled_types import CompiledTypeRepo
        for astnode in self.node_types:
            for prop in astnode.get_properties(*args, **kwargs):
                yield prop

        # Compute properties for non-astnode types
        for _, typ in CompiledTypeRepo.type_dict.items():
            if not typ.is_ast_node:
                for prop in typ.get_properties(*args, **kwargs):
                    yield prop

    @memoized
    def properties_logging(self):
        """
        Return whether logging is activated for any properties in the compile
        context.
        """
        return any(prop.activate_tracing for prop in self.all_properties)

    def check_can_reach_signature(self) -> None:
        """
        Check that the root node's "can_reach" signature is conforming.
        """
        from langkit.compiled_types import T

        # If the language spec does not create one, the initialisation of the
        # root node is supposed to create an automatic "can_reach" property.
        fields = self.root_node_type.get_abstract_node_data_dict()
        can_reach = fields["can_reach"]

        qualname = can_reach.qualname
        args = can_reach.natural_arguments

        with can_reach.diagnostic_context:
            check_source_language(
                can_reach.is_property,
                f"{qualname} must be a property",
            )
            check_source_language(
                can_reach.type.matches(T.Bool),
                f"{qualname} must return a boolean",
            )
            check_source_language(
                len(args) == 1 and args[0].type.matches(T.root_node),
                f"{qualname} must take one argument: a bare node",
            )
            check_source_language(
                not can_reach.uses_entity_info,
                f"{qualname} cannot use entities",
            )

    def compute_properties_callgraphs(self) -> None:
        """
        Compute forwards and backwards properties callgraphs.

        The forwards callgraph is a mapping::

           Caller property -> set of called properties

        While the backwards one is::

           Called property -> set of caller properties

        This takes care of overriding properties: if C calls A and B overrides
        A, then we consider that C calls both A and B. Note that this considers
        references to properties in logic expressions as calls.

        :param forwards_converter: Function to customize what the forwards call
            graph contains. It is its result that is added to the returned set.
            The given resolved expression, which comes from the caller property
            is the expression that references the given called property.
        :type forwards_converter: (ResolvedExpression, PropertyDef) -> T

        :param backwards_converter: Likewise for the backwards callgraph.
            The given resolved expression, which comes from the given caller
            property is the expression that references the called property.
        :type forwards_converter: (ResolvedExpression, PropertyDef) -> T
        """
        from langkit.expressions import PropertyDef

        def add_forward(from_prop, to_prop):
            backwards.setdefault(to_prop, set())
            forwards[from_prop].add(to_prop)
            backwards[to_prop].add(from_prop)
            for over_prop in to_prop.all_overridings:
                add_forward(from_prop, over_prop)

        def traverse_expr(expr):
            for ref_prop in expr.flat_subexprs(
                lambda e: isinstance(e, PropertyDef)
            ):
                add_forward(prop, ref_prop)
            for subexpr in expr.flat_subexprs():
                traverse_expr(subexpr)

        forwards: dict[PropertyDef, set[PropertyDef]] = {}
        backwards: dict[PropertyDef, set[PropertyDef]] = {}

        for prop in self.all_properties(include_inherited=False):
            forwards.setdefault(prop, set())
            backwards.setdefault(prop, set())

            # For dispatchers, add calls to the dispatched properties
            if prop.is_dispatcher:
                for _, static_prop in prop.dispatch_table:
                    add_forward(prop, static_prop)

            # For regular properties, add calls from the property expression
            elif prop.constructed_expr:
                traverse_expr(prop.constructed_expr)

        self.properties_forwards_callgraph = forwards
        self.properties_backwards_callgraph = backwards

    def compute_reachability(
        self,
        forward_map: dict[PropertyDef, set[PropertyDef]],
    ) -> set[PropertyDef]:
        """
        Compute the set of properties that are transitively called (according
        to the given forward map) by a public property, Predicate parsers in
        the grammar or resolvers in env specs. Also assume that all internal
        properties are reachable.
        """
        from langkit.parsers import Predicate

        reachable_set = set()

        # First compute the set of properties called by Predicate parsers
        called_by_grammar = set()

        def visit_parser(parser):
            if isinstance(parser, Predicate):
                called_by_grammar.add(parser.property_ref)
            for child in parser.children:
                visit_parser(child)

        for rule in self.grammar.rules.values():
            visit_parser(rule)

        # Consider the following internal properties as "first reachables":
        #
        # * public and internal properties;
        # * properties with "warn_on_unused" disabled;
        # * properties called by env specs.

        queue = {
            p for p in forward_map
            if p.is_public or p.is_internal or not p.warn_on_unused
        }
        queue.update(called_by_grammar)

        for astnode in self.node_types:
            if astnode.env_spec:
                for action in astnode.env_spec.actions:
                    queue.update(action.resolvers)

        # Propagate the "reachability" attribute to called properties,
        # transitively.
        while queue:
            prop = queue.pop()
            reachable_set.add(prop)
            queue.update(p for p in forward_map[prop]
                         if p not in reachable_set)

        return reachable_set

    def compute_is_reachable_attr(self) -> None:
        """
        Pass that will compute the `is_reachable` attribute for every property.
        Reachable properties are properties that transitively called by a
        public property, Predicate parsers in the grammar or resolvers in env
        specs. Also assume that all internal properties are reachable.
        """
        assert self.properties_forwards_callgraph is not None
        reachable_properties = self.compute_reachability(
            self.properties_forwards_callgraph
        )
        for prop in self.all_properties(include_inherited=False):
            prop.set_is_reachable(prop in reachable_properties)

    def compute_uses_entity_info_attr(self):
        """
        Pass that will compute the `uses_entity_info` attribute for every
        property.  This will determine whether it is necessary to pass along
        entity information or not.
        """
        from langkit.expressions import FieldAccess

        # For each property that uses entity info, extend that attribute to the
        # whole property set, as it changes the signature of the generated
        # subprograms.
        props_using_einfo = sorted(
            self.all_properties(lambda p: p._uses_entity_info,
                                include_inherited=False),
            key=lambda p: p.qualname
        )
        for prop in props_using_einfo:
            for p in prop.field_set():
                with diagnostic_context(p.location):
                    p.set_uses_entity_info()

        all_props = list(self.all_properties(include_inherited=False))

        # Then, clearly tag all properties that don't use entity info
        for prop in all_props:
            prop._uses_entity_info = bool(prop._uses_entity_info)

        # Now that we determined entity info usage for all properties, make
        # sure that calls to properties that require entity info are made on
        # entities.

        def process_expr(expr):
            if isinstance(expr, FieldAccess.Expr):
                location = (
                    expr.debug_info.location
                    if expr.debug_info else
                    Location.unknown
                )

                check_source_language(
                    not expr.node_data.uses_entity_info
                    or expr.node_data.optional_entity_info
                    or expr.implicit_deref,
                    'Call to {} must be done on an entity'.format(
                        expr.node_data.qualname
                    ),
                    location=location,
                    severity=Severity.non_blocking_error
                )

            for subexpr in expr.flat_subexprs():
                process_expr(subexpr)

        for prop in all_props:
            with prop.diagnostic_context:
                if prop.constructed_expr:
                    process_expr(prop.constructed_expr)

    def compute_uses_envs_attr(self):
        """
        Pass to compute the `uses_envs` attribute for every property.

        This will determine if public properties need to automatically call
        Populate_Lexical_Env.
        """
        queue = sorted(self.all_properties(lambda p: p._uses_envs,
                                           include_inherited=False),
                       key=lambda p: p.qualname)

        # Propagate the "uses envs" attribute in the backwards call graph
        while queue:
            prop = queue.pop(0)
            for caller in self.properties_backwards_callgraph[prop]:
                if not caller._uses_envs:
                    caller.set_uses_envs()
                    queue.append(caller)

        # For all unreached nodes, tag them as not using envs
        for prop in self.all_properties(include_inherited=False):
            prop._uses_envs = bool(prop._uses_envs)

    def warn_on_undocumented(self, node):
        """
        Emit a warning if ``node`` is not documented.
        """
        # Ignore nodes that are created during the expansion of enum nodes:
        # users cannot add documentation for these.
        if node.base and node.base.is_enum_node:
            return

        # Likewise for the very abstract generic list type
        elif node.is_generic_list_type:
            return

        WarningSet.undocumented_nodes.warn_if(
            not node._doc,
            'This node lacks documentation',
            location=node.location,
        )

    def warn_unused_private_properties(self):
        """
        Check that all private properties are actually used: if one is not,
        it is useless, so emit a warning for it.
        """
        forwards_strict = self.properties_forwards_callgraph

        # Compute the callgraph with flattened subclassing information:
        # consider only root properties.
        forwards = defaultdict(set)
        for prop, called in forwards_strict.items():
            root = prop.root
            forwards[root].update(c.root for c in called)

        # The first is for strict analysis while the second one simplifies
        # properties to their root.
        reachable_by_public_strict = self.compute_reachability(forwards_strict)
        reachable_by_public = self.compute_reachability(forwards)

        # The unused private properties are the ones that are not part of this
        # set.
        unreachable_private_strict = (
            set(forwards_strict) - reachable_by_public_strict
        )
        unreachable_private = set(forwards) - reachable_by_public
        assert all(p.is_private for p in unreachable_private_strict)

        # Now determine the set of unused abstraction: it's all root properties
        # that are unused in the strict analysis but used in the other one.
        unused_abstractions = {
            p.root for p in
            (unreachable_private_strict - unreachable_private)
        }

        def warn(unused_set, message):
            sorted_set = sorted(
                (p.qualname, p)
                for p in unused_set
                if (
                    # Never warn about implicitly referenced properties:
                    # internal properties, artificial properties and
                    # "can_reach" (can come from the language spec but used by
                    # lexical env lookups).
                    not p.is_internal
                    and not p.artificial
                    and p.indexing_name != "can_reach"
                )
            )
            for _, p in sorted_set:
                with p.diagnostic_context:
                    check_source_language(False, message,
                                          severity=Severity.warning)

        warn(unreachable_private, 'This private property is unused')
        warn(unused_abstractions, 'This private abstraction is unused')

    def warn_unreachable_base_properties(self):
        """
        Emit a warning for properties that can never be executed because:

        * they are defined on an abstract node;
        * all concrete subclasses override it;
        * they are not called through Super().
        """
        unreachable = []

        for astnode in self.node_types:
            for prop in astnode.get_properties(include_inherited=False):
                # As we process whole properties set in one round, just focus
                # on root properties. And of course only on dispatching
                # properties.
                if prop.base or not prop.dispatching:
                    continue

                # Also focus on properties for which we emit code (concrete
                # ones).
                props = [p for p in prop.field_set() if not p.abstract]

                # Set of concrete nodes that can reach this property
                nodes = set(astnode.concrete_subclasses)

                # Process properties in reverse hierarchical order to process
                # leaf properties before parent ones.
                for p in reversed(props):
                    # Compute the set of concrete subclasses that can call "p"
                    reaching_p = set(p.owner.concrete_subclasses) & nodes

                    # If this set is empty and this property isn't the target
                    # of a Super() call, then it is unreachable.
                    if not p.called_by_super and not reaching_p:
                        unreachable.append(p)

                    nodes = nodes - reaching_p

        unreachable.sort(key=lambda p: p.location)
        for p in unreachable:
            with p.diagnostic_context:
                check_source_language(
                    False,
                    'Unreachable property: all concrete subclasses override'
                    ' it',
                    severity=Severity.warning
                )

    _template_extensions_fns: list[Callable[[CompileCtx], dict[str, Any]]] = []
    """
    List of functions to create the default template environment.
    """

    _template_extensions_frozen = False
    """
    Whether at least one context has requested the list of template extensions.
    Once it's true, one cannot register template extensions anymore.
    """

    @property  # type: ignore
    @memoized
    def template_extensions(self):
        """
        Return the set of template extensions evaluated for this context.

        :rtype: dict[str, object]
        """
        CompileCtx._template_extensions_frozen = True

        from langkit.common import ascii_repr, comment_box
        assert self.emitter
        base_env = {
            'comment_box': comment_box,
            'ascii_repr':  ascii_repr,
            'Name':        names.Name,
            'ada_doc':     documentation.ada_doc(self),
            'c_doc':       documentation.c_doc(self),
            'py_doc':      documentation.py_doc(self),
            'java_doc':    documentation.java_doc(self),
            'ocaml_doc':   documentation.ocaml_doc(self),
            'ada_c_doc':   documentation.ada_c_doc(self),
            'emitter':     self.emitter,
        }
        for fn in CompileCtx._template_extensions_fns:
            ext_env = fn(self)
            for k, v in ext_env.items():
                assert k not in base_env, (
                    'Duplicate key in renderer env: {}'.format(k)
                )
                base_env[k] = v
        return base_env

    @property  # type: ignore
    @memoized
    def renderer(self):
        """
        Return the default renderer for this context.
        """
        from langkit import template_utils
        return template_utils.Renderer(self.template_extensions)

    def render_template(self, *args, **kwargs):
        """
        Shortcut for ``self.renderer.render(*args, **kwargs)``.
        """
        return self.renderer.render(*args, **kwargs)

    @classmethod
    def register_template_extensions(
        cls,
        exts_fn: Callable[[CompileCtx], dict[str, Any]]
    ) -> None:
        """
        Register a set of mako template env extensions.

        :param exts_fn: Function to be evaluated the first time the renderer is
            created.
        """
        assert not cls._template_extensions_frozen
        CompileCtx._template_extensions_fns.append(exts_fn)

    def create_all_passes(self, check_only: bool = False) -> None:
        """
        Create all the passes necessary to the compilation of the DSL. This
        should be called before ``emit``.

        :param check_only: If true, only perform validity checks: stop before
            code emission. This is useful for IDE hooks. False by default.
        """
        assert self.emitter is None

        self.check_only = check_only
        if self.config.emission.coverage:
            self.gnatcov = GNATcov(self)

        # Compute the list of passes to run:

        # First compile the DSL
        self.all_passes = self.compilation_passes

        # Then, if requested, emit code for the generated library
        if not self.check_only:
            self.all_passes += self.code_emission_passes()

        # Activate/desactive optional passes as per explicit requests
        pass_activations = dict(self.config.optional_passes)
        for p in self.all_passes:
            if p.is_optional:
                match pass_activations.pop(p.name, None):
                    case bool(enable):
                        p.disabled = not enable

        # Reject invalid pass activation requests
        for n in pass_activations:
            with diagnostic_context(Location.nowhere):
                error(f"No optional pass with name {n}")

    def emit(self):
        """
        Compile the DSL and emit sources for the generated library.
        """
        with names.camel_with_underscores, global_context(self):
            try:
                self.run_passes(self.all_passes)
                if not self.check_only and self.emitter is not None:
                    self.emitter.cache.save()
            finally:
                self.emitter = None

    def lower_lkt(self):
        """
        Run the Lkt lowering passes over Lkt input files.
        """
        from langkit.frontend.grammar import create_grammar
        from langkit.frontend.lexer import create_lexer
        from langkit.frontend.resolver import Resolver
        from langkit.frontend.types import create_types
        from langkit.frontend.utils import load_lkt

        # Parse Lkt sources now. Note that we do not do it in the CompilCtx
        # constructor because this operation is not trivial and not always
        # necessary (for instance not needed for setenv).
        self.lkt_units = load_lkt(self.config.lkt_spec)
        self.lkt_resolver = Resolver(self, self.lkt_units)

        create_types(self.lkt_resolver)
        self.lexer = create_lexer(self.lkt_resolver)
        self.grammar = create_grammar(self.lkt_resolver)

    def lower_expressions(self) -> None:
        """
        Lower Lkt expressions.
        """
        self.lkt_types_loader.lower_expressions()

    def compile(self):
        """
        Compile the DSL.
        """
        with global_context(self):
            self.run_passes(self.compilation_passes)

    @property
    def composite_types(self):
        assert self._composite_types is not None
        return self._composite_types

    @property
    def array_types(self):
        assert self._array_types is not None
        return self._array_types

    @property
    def iterator_types(self) -> list[IteratorType]:
        assert self._iterator_types is not None
        return self._iterator_types

    @property
    def struct_types(self):
        assert self._struct_types is not None
        return self._struct_types

    @property
    def entity_types(self):
        assert self._entity_types is not None
        return self._entity_types

    @property
    def enum_types(self):
        return self._enum_types

    @property
    def compilation_passes(self):
        """
        Return the list of passes to compile the DSL.
        """
        from langkit.compiled_types import create_builtin_types
        from langkit.envs import EnvSpec
        from langkit.expressions import PropertyDef
        from langkit.generic_interface import check_interface_implementations
        from langkit.lexer import Lexer
        from langkit.parsers import Grammar, Parser
        from langkit.passes import (
            ASTNodePass, EnvSpecPass, GlobalPass, GrammarPass, GrammarRulePass,
            LexerPass, MajorStepPass, PropertyPass, errors_checkpoint_pass
        )

        return [
            MajorStepPass('Lkt processing'),
            errors_checkpoint_pass,
            GlobalPass("create builtin types", create_builtin_types),
            GlobalPass('lower Lkt', CompileCtx.lower_lkt),

            MajorStepPass('Compiling the lexer'),
            LexerPass('check token families', Lexer.check_token_families),
            LexerPass('compile lexer rules', Lexer.compile_rules),

            MajorStepPass('Compiling the grammar'),
            GrammarPass('check grammar entry points',
                        Grammar.check_entry_points),
            GrammarPass('compute user defined rules',
                        Grammar.compute_user_defined_rules),
            GrammarPass('warn on unreferenced parsing rules',
                        Grammar.warn_unreferenced_parsing_rules),
            EnvSpecPass('register categories', EnvSpec.register_categories),
            GrammarRulePass('compute parser types', Parser.compute_types),
            GrammarRulePass('freeze parser types', Parser.freeze_types),
            GrammarRulePass('check type of top-level grammar rules',
                            Parser.check_toplevel_rules),

            GrammarRulePass('compute dont skip rules',
                            lambda p: p.traverse_dontskip(self.grammar)),

            # This cannot be done before as the "compute fields type" pass will
            # create AST list types.
            GlobalPass('compute types', CompileCtx.compute_types),
            ASTNodePass('check inferred field types',
                        lambda _, node: node.check_inferred_field_types()),
            ASTNodePass('validate AST node fields',
                        lambda _, astnode: astnode.validate_fields(),
                        auto_context=False),
            ASTNodePass('reject abstract AST nodes with no concrete'
                        ' subclasses', CompileCtx.check_concrete_subclasses),
            errors_checkpoint_pass,

            MajorStepPass('Compiling properties'),
            PropertyPass('compute property attributes',
                         PropertyDef.compute_property_attributes),
            GlobalPass('lower expressions', CompileCtx.lower_expressions),
            PropertyPass('construct and type expressions',
                         PropertyDef.construct_and_type_expression),
            PropertyPass('check overriding types',
                         PropertyDef.check_overriding_types),
            GlobalPass('compute properties callgraphs',
                       CompileCtx.compute_properties_callgraphs),
            GlobalPass('compute uses entity info attribute',
                       CompileCtx.compute_uses_entity_info_attr),
            GlobalPass('compute uses envs attribute',
                       CompileCtx.compute_uses_envs_attr),
            GlobalPass('check can_reach signature',
                       CompileCtx.check_can_reach_signature),
            EnvSpecPass('check env specs', EnvSpec.check_spec),
            GlobalPass('compute is reachable attribute',
                       CompileCtx.compute_is_reachable_attr),
            GlobalPass('warn on unused private properties',
                       CompileCtx.warn_unused_private_properties),
            GlobalPass('warn on unreachable base properties',
                       CompileCtx.warn_unreachable_base_properties),
            PropertyPass('warn on undocumented public properties',
                         PropertyDef.warn_on_undocumented_public_property),
            ASTNodePass('warn on undocumented nodes',
                        CompileCtx.warn_on_undocumented),
            GlobalPass('compute composite types',
                       CompileCtx.compute_composite_types),
            ASTNodePass('expose public structs and arrays types in APIs',
                        CompileCtx.expose_public_api_types,
                        auto_context=False),
            GlobalPass('check interface method implementations',
                       check_interface_implementations),
            GlobalPass('lower properties dispatching',
                       CompileCtx.lower_properties_dispatching),
            GlobalPass('check memoized properties',
                       CompileCtx.check_memoized),
            GlobalPass('compute AST node constants',
                       CompileCtx.compute_astnode_constants),

            PropertyPass("Check properties' docstrings",
                         PropertyDef.check_docstring),

            GlobalPass("Check types' docstrings",
                       CompileCtx.check_types_docstrings),

            GlobalPass('compute field nullability',
                       CompileCtx.compute_field_nullability),

            errors_checkpoint_pass,

            MajorStepPass('Computing precise types'),
            ASTNodePass('compute precise fields types',
                        lambda _, n: n.compute_precise_fields_types()),
            GlobalPass('check PLE unit root', CompileCtx.check_ple_unit_root),

            GrammarRulePass('compile parsers', Parser.compile),
            GrammarPass(
                "compute nullable parsers", self.unparsers.compute_nullability,
            ),
            GrammarRulePass('compute nodes parsers correspondence',
                            self.unparsers.compute),
            ASTNodePass('reject parser-less nodes',
                        self.unparsers.reject_parserless_nodes),
            ASTNodePass('warn imprecise field type annotations',
                        lambda _, astnode:
                        astnode.warn_imprecise_field_type_annotations()),
            GlobalPass('log node parsers correspondence',
                       self.unparsers.check_nodes_to_rules),
            GlobalPass('finalize unparsers code generation',
                       self.unparsers.finalize),
        ]

    def code_emission_passes(self) -> list[AbstractPass]:
        """
        Return the list of passes to emit sources for the generated library.
        """
        from langkit.emitter import Emitter
        from langkit.expressions import PropertyDef
        from langkit.parsers import Parser
        from langkit.passes import (
            EmitterPass, GlobalPass, GrammarRulePass, MajorStepPass,
            PropertyPass, errors_checkpoint_pass
        )
        from langkit.railroad_diagrams import emit_railroad_diagram

        def pass_fn(ctx):
            ctx.emitter = Emitter(self)

        return [
            MajorStepPass('Prepare code emission'),

            GlobalPass('prepare code emission', pass_fn),

            GrammarRulePass('register parsers symbol literals',
                            Parser.add_symbol_literals),
            # Past this point, the set of symbol literals is frozen
            GlobalPass('finalize symbol literals',
                       CompileCtx.finalize_symbol_literals),

            GrammarRulePass('render parsers code',
                            lambda p: Parser.render_parser(p, self)),
            PropertyPass('render property', PropertyDef.render_property),
            errors_checkpoint_pass,

            MajorStepPass('Generate library sources'),
            EmitterPass('setup directories', Emitter.setup_directories),

            # Run early plugin code emission passes after the directories are
            # created but yet before the project file has been emitted so they
            # can generate source files there.
            *self.plugin_passes,

            EmitterPass('merge support libraries',
                        Emitter.merge_support_libraries),
            EmitterPass('generate lexer DFA', Emitter.generate_lexer_dfa),
            EmitterPass('emit Ada sources', Emitter.emit_ada_lib),
            EmitterPass('emit mains', Emitter.emit_mains),
            EmitterPass('emit C API', Emitter.emit_c_api),
            EmitterPass('emit Python API', Emitter.emit_python_api),
            EmitterPass('emit Python playground',
                        Emitter.emit_python_playground),
            EmitterPass('emit GDB helpers', Emitter.emit_gdb_helpers),
            EmitterPass('emit OCaml API', Emitter.emit_ocaml_api),
            EmitterPass('emit Java API', Emitter.emit_java_api),
            EmitterPass('emit library project file',
                        Emitter.emit_lib_project_file),
            EmitterPass('instrument for code coverage',
                        Emitter.instrument_for_coverage),

            GrammarRulePass('emit railroad diagrams', emit_railroad_diagram)
            .optional(
                """
                Emit SVG railroad diagrams for grammar rules, in share/doc.
                Needs the railroad-diagrams Python library.
                """
            ),

            GlobalPass('report unused documentation entries',
                       lambda ctx: ctx.documentations.report_unused())
            .optional(
                """
                Report unused documentation entries. This is an internal pass
                that is used by Langkit devs to maintain the langkit
                documentation.
                """
            ),
        ]

    def run_passes(self, passes):
        """
        Run the given passes through the pass manager.

        :param list[langkit.passes.AbstractPass]: List of compilation passes to
            go through.
        """
        from langkit.passes import PassManager
        pass_manager = PassManager()
        pass_manager.add(*passes)
        pass_manager.run(self)

    @property
    def extensions_dir(self):
        """
        Return the absolute path to the extension dir, if it exists on the
        disk, or None.
        """
        return self._extensions_dir

    def ext(self, *args):
        """
        Return an extension path, relative to the extensions dir, given
        strings/names arguments, only if the extension file/dir exists, so that
        you can do::

            ext('a', 'b', 'c')
            # returns 'a/b/c'

        :param [str|names.Name] args: The list of components to constitute the
                                      extension's path.

        :rtype: str
        """
        args = [a.lower if isinstance(a, names.Name) else a for a in args]
        if self.extensions_dir:
            ret = path.join(*args)
            p = path.join(self.extensions_dir, ret)
            if path.isfile(p) or path.isdir(p):
                return ret

    def add_symbol_literal(self, name):
        """
        Add "name" to the list of symbol literals.

        This must not be called after finalize_symbol_literals is invoked.

        :type name: str
        """
        assert isinstance(self._symbol_literals, set)
        self._symbol_literals.add(name)

    @property
    def sorted_symbol_literals(self):
        """
        Return the list of symbol literals sorted in enumeration order.

        :rtype: list[(str, str)]
        """
        return sorted(self.symbol_literals.items(),
                      key=lambda kv: kv[1])

    def finalize_symbol_literals(self):
        """
        Collect all symbol literals provided to "add_symbol_literal" and create
        the "symbol_literals" mapping out of it.
        """
        assert isinstance(self._symbol_literals, set)
        symbols = self._symbol_literals
        self._symbol_literals = None

        for i, name in enumerate(sorted(symbols)):
            # Replace all non-alphabetic characters with underscores
            tmp_1 = (c if c.isalpha() else '_' for c in name.lower())

            # Remove consecutive underscores
            tmp_2 = reduce(
                lambda s, c: s if s.endswith('_') and c == '_' else s + c,
                tmp_1
            )

            # Remove leading/trailing underscores, and add 'Precomputed_Sym'
            # prefix (not 'Precomputed_Symbol' to avoid conflicts with other
            # 'Precomputed_Symbol_*' entities in the generated code).
            candidate_name = names.Name("Precomputed_Sym")
            if tmp_2.strip("_"):
                candidate_name += names.Name.from_lower(tmp_2.strip("_"))

            # If the candidate is already used, add an unique number
            if candidate_name in self.symbol_literals.values():
                candidate_name = names.Name(f"{candidate_name.base_name}_{i}")

            self.symbol_literals[name] = candidate_name

    def compute_astnode_constants(self):
        """
        Compute several constants for the current set of AST nodes.
        """
        # Compute the set of "kind" constants
        for i, astnode in enumerate(
            (astnode
             for astnode in self.node_types
             if not astnode.abstract),
            # Start with 1: the constant 0 is reserved as an
            # error/uninitialized code.
            start=1
        ):
            self.node_kind_constants[astnode] = i
            self.kind_constant_to_node[i] = astnode

        # Compute the list of parse fields and public properties, for
        # introspection. Also compute parse field indexes.
        self.sorted_parse_fields = []
        self.sorted_properties = []
        for n in self.node_types:
            i = 0
            for f in n.get_parse_fields():

                # Compute the index
                if f.abstract or f.null:
                    assert f._index in (None, -1)
                    f._index = -1
                else:
                    if f._index is None:
                        f._index = i
                    else:
                        assert f._index == i
                    i += 1

                # Register the field
                if (f.abstract or not f.is_overriding) and f.owner is n:
                    self.sorted_parse_fields.append(f)

            for p in n.get_properties(predicate=lambda p: p.is_public,
                                      include_inherited=False):
                if not p.is_overriding:
                    self.sorted_properties.append(p)

    def add_pending_node_type(self, t: ASTNodeType) -> None:
        """
        Add a node type to the queue of types to process in the
        ``compute_types`` pass.
        """
        assert self.pending_node_types is not None
        self.pending_node_types.append(t)

    def add_pending_enum_type(self, t: EnumType) -> None:
        """
        Add an enum type to the queue of types to process in the
        ``compute_types`` pass.
        """
        assert self.pending_enum_types is not None
        self.pending_enum_types.append(t)

    def add_pending_composite_type(self, t: CompiledType) -> None:
        """
        Add a composite type (struct, array or iterator) to the queue of types
        to process in the ``compute_composite_types`` pass.
        """
        assert self.pending_composite_types is not None
        self.pending_composite_types.append(t)

    def compute_composite_types(self) -> None:
        """
        Check that struct and array types are valid and compute related lists.

        Today this only checks that there is no inclusing loop between these
        types. For instance: (1) is an array of (2) and (2) is a struct that
        contains (1).
        """
        from langkit.compiled_types import (
            ArrayType,
            CompiledType,
            EntityType,
            IteratorType,
            NodeBuilderType,
            StructType,
            T,
        )

        def dependencies(typ: CompiledType) -> list[CompiledType]:
            """
            Return dependencies for the given compiled type that are relevant
            to the topological sort of composite types.
            """
            if typ.is_struct_type:
                # A struct type depends on the type of its fields
                result = [f.type for f in typ.get_fields()]

                # For non-root entity types, also add a dependency on the
                # parent entity type so that parents are declared before their
                # children.
                if typ.is_entity_type and not typ.element_type.is_root_node:
                    result.append(typ.element_type.base.entity)

            elif typ.is_array_type:
                result = [typ.element_type]

            elif typ.is_iterator_type:
                result = [typ.element_type]

            else:
                assert False, 'Invalid composite type: {}'.format(typ.dsl_name)

            # Filter types that are relevant for dependency analysis
            return [t for t in result if t.is_struct_type or t.is_array_type]

        # Collect existing types and make sure we don't create other ones later
        # by accident.
        array_types: set[ArrayType] = set()
        iterator_types: set[IteratorType] = set()
        entity_types: set[EntityType] = set()
        node_builder_types: set[NodeBuilderType] = set()
        struct_types: set[StructType] = set()
        assert self.pending_composite_types is not None
        for t in self.pending_composite_types:
            if isinstance(t, ArrayType):
                array_types.add(t)
            elif isinstance(t, IteratorType):
                iterator_types.add(t)
            elif isinstance(t, NodeBuilderType):
                node_builder_types.add(t)
            elif isinstance(t, StructType):
                struct_types.add(t)
                if isinstance(t, EntityType):
                    entity_types.add(t)
            else:
                raise AssertionError(f"invalid composite type: {t.dsl_name}")

        self.pending_composite_types = None
        self.node_builder_types = list(node_builder_types)

        # To avoid generating too much bloat, we generate C API interfacing
        # code only for iterators on root entities.  Bindings for iterators on
        # all other entity types can re-use this code, as all entity types have
        # the same ABI. This means we expose iterator for root entities as soon
        # as an iterator on any entity type is exposed.
        if any(t.element_type.is_entity_type for t in iterator_types):
            iterator_types.add(T.entity.iterator)

        def types_and_deps(types: Iterable[CompiledType]) -> list[
            tuple[CompiledType, Sequence[CompiledType]]
        ]:
            return [(t, dependencies(t)) for t in types]

        # Sort the composite types by dependency order
        all_types_and_deps: list[
            tuple[CompiledType, Sequence[CompiledType]]
        ] = [
            *types_and_deps(struct_types),
            *types_and_deps(array_types),
            *types_and_deps(iterator_types),
        ]
        try:
            self._composite_types = topological_sort(all_types_and_deps)
        except TopologicalSortError as exc:
            message = ['Invalid composition of types:']
            for i, item in enumerate(exc.loop):
                next_item = (exc.loop[i + 1]
                             if i + 1 < len(exc.loop) else
                             exc.loop[0])
                assert isinstance(item, CompiledType)
                assert isinstance(next_item, CompiledType)
                message.append('  * {} contains a {}'
                               .format(item.dsl_name, next_item.dsl_name))
            with diagnostic_context(Location.nowhere):
                error('\n'.join(message))

        # Create per-kind lists of type whose order is the same as in the
        # topo-sorted composite types list.
        self._array_types = [
            at for at in self._composite_types if isinstance(at, ArrayType)
        ]
        self._entity_types = [
            et for et in self._composite_types if isinstance(et, EntityType)
        ]
        self._iterator_types = [
            it for it in self._composite_types if isinstance(it, IteratorType)
        ]
        self._struct_types = [
            st for st in self._composite_types if isinstance(st, StructType)
        ]

    def expose_public_api_types(self, astnode):
        """
        Tag all struct and array types referenced by the public API as exposed.
        This also emits non-blocking errors for all types that are exposed in
        the public API whereas they should not.
        """
        from langkit.compiled_types import (
            ArrayType, Field, IteratorType, StructType, T
        )

        def expose(t, to_internal, for_field, type_use, traceback):
            """
            Recursively tag "t" and all the types it references as exposed.
            """
            def check(predicate, descr):
                with for_field.diagnostic_context:
                    text_tb = (
                        ' (from: {})'.format(
                            ' -> '.join(traceback[:-1])
                        ) if len(traceback) > 1 else ''
                    )
                    check_source_language(
                        predicate,
                        '{} is {}, which is forbidden in public API{}'.format(
                            type_use, descr, text_tb
                        ),
                        severity=Severity.non_blocking_error
                    )

            if t.exposed:
                # If the type is already exposed, there is nothing to *check*,
                # but we still need to set the converter flags below.
                pass

            elif t.is_entity_type:
                # Allow all entity types to be exposed, and don't try to expose
                # internals, unlike for regular structs.
                pass

            elif isinstance(t, ArrayType):
                # Don't allow public arrays of arrays
                check(
                    not isinstance(t.element_type, ArrayType),
                    '{}, an array of arrays'.format(t.dsl_name)
                )

                # Reject public arrays of bare AST nodes
                check(
                    not t.element_type.is_ast_node,
                    '{}, an array of bare AST nodes'.format(t.dsl_name)
                )

                expose(t.element_type, to_internal, for_field, 'element type',
                       traceback + ['array of {}'.format(t.dsl_name)])

            elif isinstance(t, IteratorType):
                # Reject public iterators of bare nodes
                check(
                    not t.element_type.is_ast_node,
                    f"{t.dsl_name}, an iterator on bare AST nodes",
                )

                # See processing for iterators in "compute_composite_types"
                if t.element_type.is_entity_type:
                    T.entity.iterator.exposed = True
                    T.entity.iterator._usage_forced = True

                expose(t.element_type, to_internal, for_field, 'element type',
                       traceback + ['iterator of {}'.format(t.dsl_name)])

            elif isinstance(t, StructType):
                # Expose all record fields
                for f in t.get_fields():
                    expose(f.type, to_internal, for_field, 'field type',
                           traceback + ['{} structures'.format(t.dsl_name)])
                    f.type.used_in_public_struct = True

            else:
                # Only array and struct types have their "_exposed" attribute
                # inferred. We consider all other ones to have a static value,
                # so complain if we reach a type that must not be exposed.
                check(t.exposed, t.dsl_name)
                return

            # Propagate the need of converters to exposed types. We can't rely
            # on the above recursive calls to expose if ``t`` was already
            # exposed.
            if to_internal:
                if not t.to_internal_converter_required:
                    for et in t.exposed_types:
                        expose(et, to_internal, for_field, 'exposed type',
                               traceback)
                    t.to_internal_converter_required = True
            else:
                if not t.to_public_converter_required:
                    for et in t.exposed_types:
                        expose(et, to_internal, for_field, 'exposed type',
                               traceback)
                    t.to_public_converter_required = True

            t.exposed = True

        for f in astnode.get_abstract_node_data(
            predicate=lambda f: f.is_public,
            include_inherited=False
        ):
            # Parse fields that are bare AST nodes are manually wrapped to
            # return entities in the public API. Bare AST nodes themselves
            # belong to the private API, so avoid exposing them.
            if isinstance(f, Field) and f.type.is_ast_node:
                continue

            expose(f.type, False, f,
                   'return type' if f.is_property else 'type',
                   [f.qualname])
            for arg in f.natural_arguments:
                expose(arg.type, True, f, '"{}" argument'.format(arg.dsl_name),
                       [f.qualname])
            if f.is_property:
                for dv_arg in f.dynamic_var_args:
                    dv = dv_arg.dynvar
                    expose(dv.type, True, f,
                           '"{}" dynamic variable'.format(dv.dsl_name),
                           [f.qualname])

        # Compute the list of struct public and their fields, for
        # introspection.

        self.sorted_public_structs = [t for t in self.struct_types
                                      if t.exposed and not t.is_entity_type]

        self.sorted_struct_fields = []
        for t in self.sorted_public_structs:
            self.sorted_struct_fields.extend(t.get_fields())

    def check_types_docstrings(self):
        """
        Check the docstrings of type definitions.
        """
        for astnode in self.node_types:
            with diagnostic_context(Location.for_entity_doc(astnode)):
                RstCommentChecker.check_doc(astnode._doc)

        for struct in self.struct_types:
            with diagnostic_context(Location.for_entity_doc(struct)):
                RstCommentChecker.check_doc(struct._doc)

    def lower_properties_dispatching(self):
        """
        Lower all dispatching properties.

        For each set of related dispatching properties, create a wrapper one
        that does manual dispatching based on AST node kinds and make all the
        other ones non-dispatching and private.
        """
        from langkit.compiled_types import Argument
        from langkit.expressions import (
            FieldAccess, PropertyDef, ResolvedExpression
        )

        # This pass rewrites properties, so it invalidates callgraphs
        self.properties_forwards_callgraphs = None
        self.properties_backwards_callgraphs = None

        redirected_props = {}

        # Iterate on AST nodes in hierarchical order, so that we meet root
        # properties before the overriding ones. As processing root properties
        # will remove the dispatching attribute of all overriding ones, we will
        # not process the same property twice.
        for astnode in self.node_types:
            for prop in astnode.get_properties(
                lambda p: p.dispatching,
                include_inherited=False
            ):
                # `prop` must be the ultimate base property: see the above
                # comment.
                prop_set = prop.field_set()
                assert prop_set[0] == prop

                static_props = list(prop_set)
                static_props.sort(key=lambda p: p.owner.hierarchical_name)

                # After the transformation, only the dispatching property will
                # require an untyped wrapper, so just remember if we need at
                # least one and make sure we generate at most one per property
                # hierarchy.
                requires_untyped_wrapper = any(p.requires_untyped_wrapper
                                               for p in static_props)
                for p in static_props:
                    p._requires_untyped_wrapper = False
                prop._requires_untyped_wrapper = requires_untyped_wrapper

                # The root property will be re-purposed as a dispatching
                # function, so if it wasn't abstract, create a clone that the
                # dispatcher will redirect to.
                #
                # Note that in this context, we consider abstract properties
                # with a runtime check as concrete, as we do generate a body
                # for them. Because of this, we can here create a concrete
                # property that has an abstract runtime check.
                root_static = None
                prop.is_dispatcher = True
                prop.reset_inheritance_info()

                # Assign the dispatcher a new name so that it does not conflict
                # with the root property in the generated code.
                prop_name = prop.names.codegen
                prop.codegen_name_before_dispatcher = prop_name
                prop.names.codegen = f"Dispatcher_{prop_name}"

                if not prop.abstract:
                    root_static = PropertyDef(
                        owner=prop.owner,
                        # Make sure the root property is registered under a
                        # name that is different from the dispatcher so that
                        # both are present in the structures' field dict.
                        names=dataclasses.replace(
                            prop.names,
                            index=f"[root-static]{prop.names.index}",
                            codegen=prop_name,
                        ),
                        location=prop.location,
                        expr=None,
                        type=prop.type,
                        doc=prop._raw_doc,
                        public=False,
                        arguments=[
                            Argument(
                                arg.location,
                                arg.name,
                                arg.type,
                                arg.is_artificial,
                                arg.default_value,
                            )
                            for arg in prop.natural_arguments
                        ],
                        dynamic_vars=prop.dynamic_var_args,
                        uses_entity_info=prop.uses_entity_info,
                        uses_envs=prop.uses_envs,
                        optional_entity_info=prop.optional_entity_info,
                        warn_on_unused=prop._warn_on_unused,
                        memoized=prop.memoized,
                        lazy_field=prop.lazy_field,
                    )
                    static_props[0] = root_static

                    root_static.constructed_expr = prop.constructed_expr
                    prop.constructed_expr = None

                    root_static.vars = prop.vars
                    prop.vars = None

                    root_static.location = prop.location
                    root_static.is_dispatching_root = True
                    prop.is_artificial_dispatcher = True

                    # Dispatchers themselves are never memoized: if the
                    # annotation was present on the original property, we
                    # instead propagate it to the actual root property during
                    # the above construction of "root_static".
                    prop.memoized = False

                    # Rewrite overriding properties so that Super() calls that
                    # target the root property (which is now a dispatcher) are
                    # redirected to "root_static" (the one that contains code
                    # for the actual root property).

                    def rewrite(expr: ResolvedExpression) -> None:
                        """
                        Rewrite Super() expressions in "expr", recursively.
                        """
                        if (
                            isinstance(expr, FieldAccess.Expr)
                            and expr.is_super
                            and expr.node_data == prop
                        ):
                            expr.node_data = root_static

                        for subexpr in expr.flat_resolved_subexprs():
                            rewrite(subexpr)

                    # The root property cannot use Super(), so process all
                    # other properties only.
                    for p in static_props[1:]:
                        if p.constructed_expr is not None:
                            rewrite(p.constructed_expr)

                else:
                    # If there is no runtime check for abstract properties, the
                    # set of concrete properties should cover the whole
                    # hierarchy tree. Just remove the future dispatcher from
                    # the list of statically dispatched properties.
                    static_props.pop(0)

                # Determine for each static property the set of concrete nodes
                # we should dispatch to it.
                dispatch_types, remainder = collapse_concrete_nodes(
                    prop.owner, reversed([p.owner for p in static_props]))
                assert not remainder
                prop.dispatch_table = lzip(reversed(dispatch_types),
                                           static_props)
                # TODO: emit a warning for unreachable properties earlier in
                # the compilation pipeline. Here we can see them with an empty
                # set of types in the dispatch table.

                # Make sure all static properties are private and not
                # dispatching anymore. Also remove their prefixes: their names
                # are already decorated and these properties will not be used
                # in public APIs (only dispatching ones are).
                for p in static_props:
                    p.prefix = None
                    p._is_public = False
                    p._abstract = False
                    p.reset_inheritance_info()
                    p.dispatcher = prop
                    redirected_props[p] = prop

                # Now turn the root property into a dispatcher
                prop._abstract = False
                prop.constructed_expr = None

                # If at least one property this dispatcher calls uses entity
                # info, then we must consider that the dispatcher itself uses
                # it (same for using environments). We must do this by hand
                # since by the time we run this expansion pass, these
                # attributes are already initialized by propagation through the
                # callgraph.
                prop._uses_entity_info = any(p.uses_entity_info
                                             for p in prop_set)
                prop._uses_envs = any(p.uses_envs for p in prop_set)

        # Now that all relevant properties have been transformed, update all
        # references to them so that we always call the wrapper. Note that we
        # don't have to do this for property expressions are we are supposed to
        # have already directed to root properties at resolved expression
        # construction time.
        for astnode in self.node_types:
            if astnode.env_spec:
                for env_action in astnode.env_spec.actions:
                    env_action.rewrite_property_refs(redirected_props)

    def generate_actions_for_hierarchy(
        self,
        node_var: str | None,
        kind_var: str,
        actions_for_node: Callable[[ASTNodeType, str | None], str],
        public_nodes: bool = False,
        unref_if_empty: list[str] | None = None,
    ) -> str:
        """
        Generate a sequence of Ada statements/nested CASE blocks to execute
        some actions on a node depending on its kind.

        This method is useful to avoid generating the same statements over and
        over for multiple node kinds. For instance, given a root node ``A`` a
        field ``f`` on ``A``, and its derivations ``B``, ``C`` and ``D``, if
        one wants to perform clean-up on some node fields, there is no need to
        generate specific code for ``B.f``, ``C.f`` and ``D.f`` when we could
        just generate code for ``A.f``.

        :param node_var: Name of the variable that holds the node to process.
            None if the generated code must work only on the kind.
        :param kind_var: Name of the variable that holds the kind of the node
            to process. Holding it in a variables is handy to avoid computing
            it multiple times.
        :param actions_for_node: Function that takes a specific node type to
            process and the name of the variable to hold the node instance to
            process. It must return the actions (i.e. Ada statements as a
            single string) to perform for this node.  Note that these actions
            should be specific to the node type, i.e. they should not overlap
            with actions for any parent node.
        :param public_nodes: Whether ``node_var`` is a type from the public
            API. Otherwise (the default), assume it is an internal node.
        :param unref_if_empty: List of names for which to generate an
            Unreferenced pragma if there are no action.
        """

        class Matcher:
            """
            Holder for "when ... =>" clauses in a CASE block.
            """

            def __init__(self, node: ASTNodeType, actions: str):
                self.node = node
                """
                Node that ``self`` matches.
                """

                self.actions = actions
                """
                List of actions specific to this matched node.
                """

                self.inner_case = Case(node)
                """
                Case instance for nodes that are more specific than ``node``.
                """

            @staticmethod
            def new_node_var(node: ASTNodeType) -> str:
                """
                Return the variable name that will hold the casted value for
                the matched node.
                """
                return f"N_{node.name}"

        class Case:
            """
            Holder for a generated CASE blocks.
            """

            def __init__(self, node: ASTNodeType):
                self.node = node
                """
                Most specific type for this CASE block's input expression.
                """

                self.matchers: list[Matcher] = []
                """
                List of matchers for this CASE block.
                """

        result: list[str] = []
        """
        List of strings for the sequence of Ada statements to return.
        """

        case_stack = [Case(self.root_node_type)]
        """
        Stack of Case instances for the Case tree we are currently building.
        First element is for the top-level CASE node while the last element is
        for the currently inner-most CASE node.
        """

        unref_names: list[str] = []
        """
        List of names to include in a "pragma Unreferenced".
        """

        def build_cases(node: ASTNodeType) -> None:
            """
            Build the tree of CASE blocks for ``node`` and all its subclasses.
            """
            # Don't bother processing classes unless they actually have
            # concrete subclasses, otherwise we would be producing dead code.
            if not node.concrete_subclasses:
                return

            to_pop = False

            if node == self.root_node_type:
                # As a special case, emit actions for the root node outside of
                # the top-level CASE block as we don't need to dispatch on
                # anything for them: they always must be applied.
                actions = actions_for_node(node, node_var)
                if actions:
                    result.append(actions)

            else:
                # If there are actions for this node, add a matcher for them
                # and process the subclasses in a nested CASE block.
                actions = actions_for_node(node, Matcher.new_node_var(node))
                if actions:
                    m = Matcher(node, actions)
                    case_stack[-1].matchers.append(m)
                    case_stack.append(m.inner_case)
                    to_pop = True

            for subcls in node.subclasses:
                build_cases(subcls)

            if to_pop:
                case_stack.pop()

        def print_case(case: Case, node_var: str | None) -> None:
            """
            Render a tree of CASE blocks and append them to ``result``.

            :param case: CASE block to render.
            :param node_var: Name of the variable that holds the node on which
                this CASE must dispatch.
            """
            if not case.matchers:
                return

            result.append(
                f"case {case.node.ada_kind_range_name} ({kind_var}) is"
            )
            for m in case.matchers:
                result.append(f"when {m.node.ada_kind_range_name} =>")
                if node_var is None:
                    new_node_var = None
                else:
                    new_node_type = (
                        m.node.entity.api_name.camel_with_underscores
                        if public_nodes else
                        m.node.name.camel_with_underscores)
                    new_node_var = m.new_node_var(m.node)

                    # Declare a new variable to hold the node subtype to
                    # process in this matcher.
                    new_node_expr = (f"{node_var}.As_{new_node_type}"
                                     if public_nodes else
                                     node_var)
                    result.append('declare')

                    # Public node names sometimes clash with introspection
                    # enumerations. Adding namespace helps generating correct
                    # code.
                    namespace = "Analysis." if public_nodes else ""
                    result.append(
                        f"{new_node_var} : constant"
                        f" {namespace}{new_node_type} := {new_node_expr};"
                    )
                    result.append('begin')

                result.append(m.actions)
                print_case(m.inner_case, new_node_var)
                if node_var is not None:
                    result.append('end;')

            result.append('when others => null;')
            result.append('end case;')

        with names.camel_with_underscores:
            build_cases(self.root_node_type)

            assert len(case_stack) == 1
            root_case = case_stack[0]

            print_case(root_case, node_var)

        # If we have no action or actions for the root node only, we have no
        # case statement, and thus the variable for the node kind is unused.
        if (
            not root_case.matchers
            or (
                len(root_case.matchers) == 1
                and root_case.matchers[0].node == self.root_node_type
            )
        ):
            unref_names.append(kind_var)

        # If we have no action at all, mark the requested variables as
        # empty.
        if not root_case.matchers and unref_if_empty:
            unref_names.extend(unref_if_empty)

        # If needed, generate the Unreferenced pragma
        if unref_names:
            result.append("pragma Unreferenced ({});".format(
                ", ".join(unref_names)
            ))

        return '\n'.join(result) or 'null;'

    @property
    def has_memoization(self):
        """
        Return whether one property is memoized.

        :rtype: bool
        """
        has_keys = bool(self.memoization_keys)
        has_values = bool(self.memoization_values)
        assert has_keys == has_values, (
            'Either there is no memoized property, either we do, in which case'
            ' there must be at least one key type and one key value'
        )
        return has_keys

    def check_memoized(self):
        """
        Check that various invariants for memoized properties are respected.
        Also register involved types in the memoization machinery.
        """
        from langkit.compiled_types import T

        class Annotation:
            """
            Analysis annotation for a property.
            """

            def __init__(self, reason=None, call_chain=[]):
                """
                :param str|None reason: None if this property can be memoized.
                    Otherwise, error message to indicate why.
                :param list[PropertyDef] call_chain: When this property cannot
                    be memoized because of transitivity, chain of properties
                    that led to this decision.
                """
                assert (reason is None) == (not call_chain)
                self.reason = reason
                self.call_chain = call_chain

            @property
            def memoizable(self):
                """
                Return whether the property this annotation relates to is
                considered to be memoizable (for now).

                :rtype: bool
                """
                return self.reason is None

            def with_call(self, prop):
                """
                Return a copy of `self` with `prop` appened to its call chain.

                :rtype: Annotation
                """
                if self.reason is None:
                    return self
                else:
                    return Annotation(self.reason, self.call_chain + [prop])

            def simpler_than(self, other):
                """
                Return whether `self` is at least as simple than `other`.

                This assumes that both `self.memoizable` and `other.memoizable`
                are false.  An annotation is consider simpler if its call chain
                is not bigger.

                :rtype: bool
                """
                assert not self.memoizable and not other.memoizable
                return (other.reason is not None and
                        len(self.call_chain) <= len(other.call_chain))

            def __repr__(self):
                return '<Annotation {} ({})>'.format(
                    self.memoizable,
                    ', '.join(p.qualname for p in self.call_chain)
                )

        back_graph = self.properties_backwards_callgraph
        annotations = {prop: Annotation() for prop in back_graph}

        # First check that properties can be memoized without considering
        # callgraph-transitive evidence that they cannot (but collect all
        # information).
        for astnode in self.node_types:
            for prop in astnode.get_properties(include_inherited=False):
                with prop.diagnostic_context:

                    tr_reason = prop.transitive_reason_for_no_memoization
                    if tr_reason is not None and not prop.call_memoizable:
                        annotations[prop] = Annotation(tr_reason, [prop])

                    if not prop.memoized:
                        continue

                    reason = prop.reason_for_no_memoization
                    check_source_language(reason is None, reason)

                    self.memoized_properties.add(prop)
                    prop.owner.add_as_memoization_key(self)
                    if prop.uses_entity_info:
                        T.EntityInfo.add_as_memoization_key(self)
                    for arg in prop.arguments:
                        check_source_language(
                            arg.type.hashable,
                            'This property cannot be memoized because argument'
                            ' {} (of type {}) is not hashable'.format(
                                arg.name.lower, arg.type.dsl_name
                            ),
                        )
                        arg.type.add_as_memoization_key(self)
                    prop.type.add_as_memoization_value(self)

        # Now do the propagation of callgraph-transitive evidence
        queue = {p for p, a in annotations.items() if not a.memoizable}
        while queue:
            callee = queue.pop()
            for caller in back_graph[callee]:
                callee_annot = annotations[callee].with_call(caller)
                caller_annot = annotations[caller]

                # Stop propagation on properties that state that they can
                # handle memoization safety.
                if caller.call_memoizable:
                    continue

                if (not callee_annot.memoizable and
                        (caller_annot.memoizable or
                            callee_annot.simpler_than(caller_annot))):
                    annotations[caller] = callee_annot
                    queue.add(caller)

        for prop, annot in sorted(annotations.items(),
                                  key=lambda p: p[0].qualname):
            if not prop.memoized or annot.memoizable:
                continue

            message = 'Property cannot be memoized '
            if annot.call_chain:
                message += '(in {}: {}, call chain is: {})'.format(
                    annot.call_chain[0].qualname,
                    annot.reason,
                    ' -> '.join(p.qualname for p in reversed(annot.call_chain))
                )
            else:
                message += '({})'.format(annot.reason)

            non_blocking_error(message, location=prop.location)

    TypeSet = utils.TypeSet

    astnode_kind_set = utils.astnode_kind_set

    collapse_concrete_nodes = staticmethod(
        utils.collapse_concrete_nodes
    )
