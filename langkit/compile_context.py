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
from functools import reduce
import importlib
import os
from os import path
from typing import (Any, Callable, Dict, List, Optional, Set, TYPE_CHECKING,
                    Tuple, Union, cast)

from funcy import lzip

from langkit import documentation, names, utils
from langkit.ada_api import AdaAPISettings
from langkit.c_api import CAPISettings
from langkit.coverage import GNATcov
from langkit.diagnostics import (
    DiagnosticError, Location, Severity, WarningSet, check_source_language,
    context_stack, diagnostic_context, error, print_error,
    print_error_from_sem_result
)
from langkit.utils import (TopologicalSortError, collapse_concrete_nodes,
                           memoized, memoized_with_default, topological_sort)


if TYPE_CHECKING:
    from langkit.compiled_types import (
        ASTNodeType, ArrayType, CompiledType, EntityType, EnumType, Field,
        IteratorType, StructType, UserField,
    )
    from langkit.emitter import Emitter
    from langkit.expressions import PropertyDef
    from langkit.lexer import Lexer
    from langkit.lexer.regexp import NFAState
    from langkit.ocaml_api import OCamlAPISettings
    from langkit.passes import AbstractPass
    from langkit.parsers import GeneratedParser, Grammar, Parser
    from langkit.python_api import PythonAPISettings


compile_ctx: Optional[CompileCtx] = None

try:
    import liblktlang as L
except ImportError:
    pass


def get_context_or_none() -> Optional[CompileCtx]:
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


ADA_SPEC = "spec"
ADA_BODY = "body"


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


class UnparseScript:
    """
    Sequence of actions to generate concrete syntax DSL.
    """

    def __init__(self, spec):
        self.actions = self.parse(spec)

    @staticmethod
    def parse(spec):
        result = []

        # Do the parsing itself
        for action in spec.split(','):
            if ':' not in action:
                key = action
                value = None
            else:
                key, value = action.split(':', 1)
            result.append((key, value))

        # Validate actions
        if not len(result):
            raise ValueError('At least one action expected')
        for i, (action, arg) in enumerate(result):
            if i == 0 and action != 'to':
                raise ValueError('First action must be to:')

            if action in ('to', 'import'):
                if arg is None:
                    raise ValueError('Missing argument for {}:'.format(action))
            elif action in ('nodes', 'lexer', 'grammar'):
                if arg is not None:
                    raise ValueError('Unexpected argument for {}:'
                                     .format(action))
            else:
                raise ValueError('Unknown action: {}:'.format(action))

        return result


class LibraryEntity:
    """
    Reference to an entity in the generated library.
    """
    def __init__(self, unit_fqn, entity_name):
        """
        Create a reference to an entity in the generated library.

        :param str unit_fqn: Fully qualified name for the unit that contains
            the referenced entity. For instance: "Libfoolang.My_Unit".
        :param str entity_name: Simple name for the entity that is referenced.
        """
        self.unit_fqn = unit_fqn
        self.entity_name = entity_name

    @property
    def fqn(self):
        """
        Fully qualified name for the referenced entity.

        For instance: "Libfoolang.My_Unit.My_Entity".

        :rtype: str
        """
        return '{}.{}'.format(self.unit_fqn, self.entity_name)


class GeneratedException:
    """
    Describe an exception in generated libraries.
    """

    def __init__(self,
                 doc_section: str,
                 package: List[names.Name],
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
        return '{}.{}'.format('.'.join(str(p) for p in self.package),
                              self.name)

    @property
    def kind_name(self) -> names.Name:
        """
        Return the enumeration name corresponding to an exception.

        :rtype: names.Name
        """
        return names.Name('Exception') + self.name


class CompileCtx:
    """State holder for native code emission."""

    c_api_settings: CAPISettings
    python_api_settings: PythonAPISettings
    ocaml_api_settings: OCamlAPISettings

    all_passes: List[AbstractPass]
    """
    List of all passes in the Langkit compilation pipeline.
    """

    check_only: bool
    """
    Whether this context is configured to only run checks on the language spec.
    """

    def __init__(self,
                 lang_name: str,
                 lexer: Optional[Lexer],
                 grammar: Optional[Grammar],
                 lib_name: Optional[str] = None,
                 short_name: Optional[str] = None,
                 c_symbol_prefix: Optional[str] = None,
                 default_charset: str = 'utf-8',
                 default_tab_stop: int = 8,
                 verbosity: Verbosity = Verbosity('none'),
                 template_lookup_extra_dirs: Optional[List[str]] = None,
                 default_unit_provider: Optional[LibraryEntity] = None,
                 case_insensitive: bool = False,
                 symbol_canonicalizer: Optional[LibraryEntity] = None,
                 documentations: Optional[Dict[str, str]] = None,
                 show_property_logging: bool = False,
                 lkt_file: Optional[str] = None,
                 types_from_lkt: bool = False,
                 lkt_semantic_checks: bool = False,
                 version: Optional[str] = None,
                 build_date: Optional[str] = None):
        """Create a new context for code emission.

        :param lang_name: string (mixed case and underscore: see
            langkit.names.Name) for the Name of the target language.

        :param lexer: A lexer for the target language.

        :param grammar: A grammar for the target language. If left to None,
            fetch the grammar in the Lktlang source.

        :param lib_name: If provided, must be a string (mixed case and
            underscore: see langkit.names.Name), otherwise set to
            "Lib<lang_name>lang". It is used for the filenames, package names,
            etc.  in the generated library.

        :param short_name: If provided, must be a string (mixed case and
            underscore: see langkit.names.Name). It will be used where a short
            name for the library is requested, for instance for the shortcut
            module name in the generated playground script.

        :param c_symbol_prefix: Valid C identifier used as a prefix for all
            top-level declarations in the generated C API.  If not provided,
            set to the name of the language in lower case.  Empty string stands
            for no prefix.

        :param default_charset: In the generated library, this will be the
            default charset to use to scan input source files.

        :param default_tab_stop: Tabulation stop to use as a default value in
            the analysis context constructor.

        :param verbosity: Amount of messages to display on standard output.
            None by default.

        :param template_lookup_extra_dirs: A list of extra directories to add
            to the directories used by mako for template lookup. This is useful
            if you want to render custom code as part of the compilation
            process.

        :param default_unit_provider: If provided, define a
            Langkit_Support.Unit_Files.Unit_Provider_Access object. This object
            will be used as the default unit provider during the creation of an
            analysis context.

            If None, this disables altogether the unit provider mechanism in
            code generation.

        :param symbol_canonicalizer: If provided, define a subprogram to call
            in order to canonicazie symbol identifiers. Such a suprogram must
            have the following signature::

                function Canonicalize
                  (Name : Text_Type) return Symbolization_Result;

            It takes an identifier name and must return the canonical name for
            it (or an error), so that all equivalent symbols have the same
            canonical name.

            This can be used, for instance, to implement case insensivity.

        :param case_insensitive: Whether to process sources as consider as case
            insensitive in the generated library. Note that this provides a
            default symbol canonicalizer that takes care of case folding
            symbols.

        :param documentations: If provided, supply templates to document
            entities. These will be added to the documentations available in
            code generation: see langkit.documentation.

        :param show_property_logging: If true, any property that has been
            marked with tracing activated will be traced on stdout by default,
            without need for any config file.

        :param lkt_file: Optional name of the file to contain Lktlang
            definitions for this language.

        :param types_from_lkt: When loading definitions from Lktlang files,
            whether to load type definitions. This is not done by default
            during the transition from our Python DSL to Lktlang.

        :param lkt_semantic_checks: Whether to force Lkt semantic checks (by
            default, enabled only if ``types_from_lkt`` is true).

        :param version: String for the version of the generated library.  This
            is "undefined" if left to None.

        :param build_date: String for the generated library build date (where
            "build" includes source generation). This is "undefined" if left to
            None.
        """
        from langkit.python_api import PythonAPISettings
        from langkit.ocaml_api import OCamlAPISettings
        from langkit.unparsers import Unparsers

        self.lang_name = names.Name(lang_name)
        self.version = version
        self.build_date = build_date

        self.lib_name = (
            names.Name('Lib{}lang'.format(self.lang_name.lower))
            if lib_name is None else
            names.Name(lib_name)
        )
        self.short_name = names.Name(short_name) if short_name else None
        self.short_name_or_long = self.short_name or self.lib_name

        self.ada_api_settings = AdaAPISettings(self)
        self.c_api_settings = CAPISettings(
            self,
            (self.lang_name.lower
             if c_symbol_prefix is None else c_symbol_prefix)
        )
        self.c_api_settings.lib_name = self.lib_name.lower

        self.default_charset = default_charset
        self.default_tab_stop = default_tab_stop

        self.verbosity = verbosity

        self.compiled = False
        """
        Whether the language specification was compiled. This is used to avoid
        doing it multiple times.

        :type: bool
        """

        self.lkt_units: List[L.AnalysisUnit] = []
        if lkt_file is None:
            assert grammar, 'Lkt spec required when no grammar is provided'
        else:
            from langkit.lkt_lowering import load_lkt
            self.lkt_units = load_lkt(lkt_file)

        self.lexer = lexer
        ":type: langkit.lexer.Lexer"

        self.grammar = grammar
        ":type: langkit.parsers.Grammar"

        self.python_api_settings = PythonAPISettings(self, self.c_api_settings)
        self.types_from_lkt = types_from_lkt
        self.lkt_semantic_checks = lkt_semantic_checks or types_from_lkt

        self.ocaml_api_settings = OCamlAPISettings(self, self.c_api_settings)

        self.fns: Set[Parser] = set()
        """
        Set of names (names.Name instances) for all generated parser
        functions. This is used to avoid generating these multiple times.
        """

        self._enum_types: List[EnumType] = []
        """
        List of all enumeration types.
        """

        self.astnode_types: List[ASTNodeType] = []
        """
        List for all ASTnodeType instances, sorted so that A is before B when A
        is a parent class for B. This sorting is important to output
        declarations in dependency order.

        This is computed right after field types inference.
        """

        self.synthetic_nodes: Optional[List[ASTNodeType]] = None
        """
        Sub-sequence of `self.astnode_types` for all nodes that are synthetic.

        This is computed right after `self.astnode_types`.
        """

        self.node_kind_constants: Dict[ASTNodeType, int] = {}
        """
        Mapping: ASTNodeType concrete (i.e. non abstract) instance -> int,
        associating specific constants to be used reliably in bindings.  This
        mapping is built at the beginning of code emission.
        """

        self.kind_constant_to_node: Dict[int, ASTNodeType] = {}
        """
        Reverse mapping for `node_kind_constants`.
        """

        self._struct_types: Optional[List[StructType]] = None
        """
        List of all plain struct types.
        """

        self._entity_types: Optional[List[EntityType]] = None
        """
        List of all entity types.
        """

        self.root_grammar_class: Optional[ASTNodeType] = None
        """
        The ASTNodeType instance that is the root class for every node used in
        the grammar.
        """

        self.generic_list_type: Optional[ASTNodeType] = None
        """
        The root gammar class subclass that is the base class for all
        automatically generated root list types.
        """

        self.env_metadata: Optional[StructType] = None
        """
        The StructType instance that will be used as the lexical environment
        metadata type.
        """

        self.list_types: Set[ASTNodeType] = set()
        """
        Set of all ASTNodeType instances for which we generate a corresponding
        list type.
        """

        self.exception_types: Dict[str, GeneratedException] = {}
        """
        Mapping of all exception types. Keys are lower-case exception names.
        """

        self._array_types: Optional[List[ArrayType]] = None
        """
        Sorted list of all ArrayType instances.

        For each ArrayType instance T, code emission for type definition will
        automatically happen.
        """

        self._iterator_types: Optional[List[IteratorType]] = None
        """
        List of all IteratorType instances.
        """

        self._composite_types: Optional[List[CompiledType]] = None
        """
        Dependency-sorted list of array and struct types.
        """

        self.memoized_properties: Set[PropertyDef] = set()
        """
        Set of all PropertyDef instances that are memoized.
        """

        self.memoization_keys: Set[CompiledType] = set()
        """
        Set of all CompiledType instances that are used as key in the hashed
        maps used to implement properties memoization. All of them must be
        hashable.
        """

        self.memoization_values: Set[CompiledType] = set()
        """
        Set of all CompiledType instances that are used as value in the hashed
        maps used to implement properties memoization. Any type can fit, there
        is no restriction.
        """

        self.symbol_literals: Dict[str, names.Name] = {}
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

        self._symbol_literals: Set[str] = set()
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

        self.generated_parsers: List[GeneratedParser] = []

        self._extensions_dir: Optional[str] = None
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

        self.template_lookup_extra_dirs: List[str] = (
            template_lookup_extra_dirs or []
        )

        self.additional_source_files: List[str] = []
        """
        List of path for file names to include in the generated library.
        """

        self.logic_binders: Set[Tuple[
            Optional[PropertyDef],
            Optional[PropertyDef]
        ]] = set()
        """
        Set of tuple of properties for which we want to generate logic binders.
        For each binder, there are potentially two properties: the conversion
        property and the equality property. See langkit.expressions.logic.Bind
        for more information.
        """

        self.default_unit_provider = default_unit_provider
        self.case_insensitive = case_insensitive
        self.symbol_canonicalizer = symbol_canonicalizer
        if self.symbol_canonicalizer is None and self.case_insensitive:
            self.symbol_canonicalizer = LibraryEntity(
                "Langkit_Support.Symbols", "Fold_Case"
            )

        docs = dict(documentation.base_langkit_docs)
        if documentations:
            docs.update(documentations)
        self.documentations = documentation.instantiate_templates(docs)
        """
        Documentation database. Associate a Mako template for each entity to
        document in the generated library.
        """

        self.parsers_varcontext_stack: List[str] = []
        """
        Holder for the stack of variables contexts used in parsers code
        emission.
        """

        self.warnings = WarningSet()
        """
        Set of warnings to emit.
        """

        self.with_clauses: Dict[
            Tuple[str, str],
            List[Tuple[str, bool, bool]]
        ] = defaultdict(list)
        """
        Mapping that binds a list of additional WITH/USE clauses to generate
        for each source file in the generated library. Used to add WITH/USE
        clauses required by extensions. See the `add_with_clause` method.
        """

        self.sorted_public_structs: Optional[List[StructType]] = None
        """
        Sorted list of all public structs. Used to generate the introspection
        API.

        Note that this excludes the entity type: the fact that entities are
        structs is an implementation detail, not exposed to public APIs.
        """

        self.sorted_struct_fields: Optional[List[UserField]] = None
        """
        Sorted list of all public fields for structs in
        ``self.sorted_public_structs``. Used to generate the introspection API.
        """

        self.sorted_parse_fields: Optional[Field] = None
        """
        Sorted list of all parsing fields, minus fields that override abstract
        ones. Used to generate the AST node introspection API.
        """

        self.sorted_properties: Optional[List[PropertyDef]] = None
        """
        Sorted list of public properties. Used to generate the property
        introspection API.
        """

        # Optional callbacks to post-process the content of source files
        self.post_process_ada: Optional[Callable[[str], str]] = None
        self.post_process_cpp: Optional[Callable[[str], str]] = None
        self.post_process_python: Optional[Callable[[str], str]] = None
        self.post_process_ocaml: Optional[Callable[[str], str]] = None

        self.ref_cats = {names.Name.from_lower('nocat')}
        """
        Set of all env lookup categories, used to optionally discriminate
        referenced envs during env lookup.
        """

        self.nfa_start: Optional[NFAState] = None
        """
        Intermediate representation for the lexer state machine (NFA).
        """

        self.unparsers: Unparsers = Unparsers(self)
        """
        :type: langkit.unparsers.Unparsers
        """

        self.emitter: Optional[Emitter] = None
        """
        During code emission, corresponding instance of Emitter. None the rest
        of the time.
        """

        self.gnatcov: Optional[GNATcov] = None
        """
        During code emission, GNATcov instance if coverage is enabled. None
        otherwise.

        :type: None|langkit.coverage.GNATcov
        """

        self.show_property_logging = show_property_logging

        # Register builtin exception types
        self._register_builtin_exception_types()

        self.properties_forwards_callgraph: \
            Optional[Dict[PropertyDef, Set[PropertyDef]]] = None
        """
        Mapping from caller properties to sets of called properties. None when
        not yet computed or invalidated.
        """

        self.properties_backwards_callgraph: \
            Optional[Dict[PropertyDef, Set[PropertyDef]]] = None
        """
        Mapping from called properties to sets of caller properties. None when
        not yet computed or invalidated.
        """

    def set_versions(self,
                     version: Optional[str] = None,
                     build_date: Optional[str] = None) -> None:
        """
        Set version numbers for the generated library. Left unchanged if None.
        """
        if version is not None:
            if self.version is not None:
                print(f"Got conflicting versions:"
                      f" {repr(version)} and {repr(self.version)}")
                raise DiagnosticError()
            self.version = version
        if build_date is not None:
            if self.build_date is not None:
                print(f"Got conflicting build dates:"
                      f" {repr(build_date)} and {repr(self.build_date)}")
                raise DiagnosticError()
            self.build_date = build_date

    @property
    def actual_version(self) -> str:
        return self.version or "undefined"

    @property
    def actual_build_date(self) -> str:
        return self.build_date or "undefined"

    @contextmanager
    def lkt_context(self, lkt_node: L.LktNode):
        """
        Context manager to set the diagnostic context to the given node.

        :param lkt_node: Node to use as a reference for this diagnostic
            context.
        """
        # Invalid type passed here will fail much later and only if a
        # check_source_language call fails. To ease debugging, check that
        # "lkt_node" has the right type here.
        assert isinstance(lkt_node, L.LktNode)

        context_stack.append(Location.from_lkt_node(lkt_node))

        try:
            yield
        finally:
            context_stack.pop()

    @staticmethod
    def lkt_doc(full_decl):
        """
        Return the documentation attached to the ``full_decl`` node, if any.

        :param liblktlang.FullDecl full_decl: Declaration to process.
        :rtype: None|str
        """
        return '\n'.join(l.text for l in full_decl.f_doc)

    def register_exception_type(self,
                                package: List[names.Name],
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
                ADA_BODY,
                ".".join(n.camel_with_underscores for n in package)
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
            ('introspection', 'bad_type_error'),
            ('introspection', 'out_of_bounds_error'),
            ('rewriting', 'template_format_error'),
            ('rewriting', 'template_args_error'),
            ('rewriting', 'template_instantiation_error')
        ]:
            doc_section = 'langkit'
            package = [names.Name('Langkit_Support'),
                       names.Name('Errors')]
            if namespace:
                doc_section = '{}.{}'.format(doc_section, namespace)
                package.append(names.Name.from_lower(namespace))

            self.register_exception_type(
                package,
                names.Name.from_lower(exception_name),
                doc_section,
                is_builtin=True
            )

        # Make original exception declarations available to exceptions handlers
        # in the C API.
        self.add_with_clause(
            "Implementation.C", ADA_BODY, "Langkit_Support.Errors"
        )

    @property
    def exceptions_by_section(self) -> List[Tuple[Optional[str],
                                                  List[GeneratedException]]]:
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
        :param str source_kind: Kind of source file in which the WITH clause
            must be added. Must be eiher ADA_SPEC or ADA_BODY.
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

    @property
    def sorted_logic_binders(self):
        return sorted(self.logic_binders, key=lambda x: (
            x[0].name.camel if x[0] else ""
            + x[1].name.camel if x[1] else ""
        ))

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
    def sorted_exception_types(self) -> List[GeneratedException]:
        """
        Turn "exception_types" into a sorted list.

        This is required during code generation to preserve a stable output.
        """
        return sorted(self.exception_types.values(),
                      key=lambda e: e.doc_entity)

    def do_generate_logic_binder(self, convert_property=None,
                                 eq_property=None):
        """
        Generate a logic binder with the given conversion property.

        If you call this function several times for the same property, only one
        binder will be generaed.

        :param PropertyDef convert_property: The conversion property.
        :param PropertyDef eq_property: The equality property.
        """
        self.logic_binders.add((convert_property, eq_property))

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
        from langkit.compiled_types import (CompiledTypeRepo, EnumType,
                                            StructType, T, resolve_type)
        from langkit.dsl import _StructMetaclass
        from langkit.expressions.base import construct_compile_time_known

        # Make sure the language spec tagged at most one metadata struct.
        # Register it, if there is one.
        user_env_md = None
        for st in _StructMetaclass.struct_types:
            if st._is_env_metadata:
                assert user_env_md is None
                user_env_md = st._type

        # If the language spec provided no env metadata struct, create a
        # default one.
        if user_env_md is None:
            CompiledTypeRepo.env_metadata = StructType(
                names.Name('Metadata'), None, None, []
            )
        else:
            CompiledTypeRepo.env_metadata = user_env_md
        self.check_env_metadata(CompiledTypeRepo.env_metadata)

        # Get the list of ASTNodeType instances from CompiledTypeRepo
        entity = CompiledTypeRepo.root_grammar_class.entity

        self.astnode_types = list(CompiledTypeRepo.astnode_types)
        self.list_types.update(
            t.element_type for t in CompiledTypeRepo.pending_list_types
        )

        self.generic_list_type = self.root_grammar_class.generic_list_type
        self.env_metadata = CompiledTypeRepo.env_metadata

        # The Group lexical environment operation takes an array of lexical
        # envs, so we always need to generate the corresponding array type.
        CompiledTypeRepo.array_types.add(T.LexicalEnv.array)

        # Likewise for the entity array type (LexicalEnv.get returns it) and
        # for the root node array type (some primitives need that).
        CompiledTypeRepo.array_types.add(entity.array)
        CompiledTypeRepo.array_types.add(
            CompiledTypeRepo.root_grammar_class.array)

        # Sort them in dependency order as required but also then in
        # alphabetical order so that generated declarations are kept in a
        # relatively stable order. This is really useful for debugging
        # purposes.
        def node_sorting_key(n: ASTNodeType) -> str:
            return n.hierarchical_name

        self.astnode_types.sort(key=node_sorting_key)

        # Also sort ASTNodeType.subclasses lists
        for n in self.astnode_types:
            n.subclasses.sort(key=node_sorting_key)

        self.synthetic_nodes = [n for n in self.astnode_types
                                if n.synthetic]

        # We need a hash function for the metadata structure as the
        # Langkit_Support.Lexical_Env generic package requires it.
        T.env_md.require_hash_function()

        # We expose a hash function for public entities, so we must generate
        # the underlying required helpers.
        T.entity.require_hash_function()

        # Create the type for grammar rules
        EnumType(name='GrammarRule',
                 location=None,
                 doc="Gramar rule to use for parsing.",
                 value_names=[self.grammar_rule_api_name(n)
                              for n in self.grammar.user_defined_rules],
                 is_builtin_type=True)

        # Force the creation of several types, as they are used in templated
        # code.
        for t in (
            # The env assoc types are required by Lexical_Env instantiation and
            # always-emitted PLE helpers.
            T.env_assoc, T.inner_env_assoc, T.inner_env_assoc.array,

            # Arrays of symbols are required to deal with environment names
            T.Symbol.array,

            # The String_To_Symbol helper obviously relies on the string type
            T.String
        ):
            _ = resolve_type(t)

        # Now that all types are known, construct default values for fields
        for st in CompiledTypeRepo.struct_types:
            for f in st.get_abstract_node_data():
                if f.abstract_default_value is not None:
                    f.default_value = construct_compile_time_known(
                        f.abstract_default_value
                    )

    def compute_optional_field_info(self):
        """
        For every parse field, find out if it is an optional field or not, i.e.
        whether it is ever produced from a parser of the user grammar that can
        create a null node.
        """
        from langkit.parsers import (Defer, DontSkip, List, Null, Opt, Or,
                                     Predicate, Skip, _Extract, _Transform)

        @memoized_with_default(False)
        def can_produce_null(parser):
            if isinstance(parser, Opt):
                # If parser is an Opt parser and is not set to produce an enum
                # alternative, it means that field is optional.
                return not parser._booleanize
            elif isinstance(parser, Null):
                return True
            elif isinstance(parser, Or):
                return any(can_produce_null(p) for p in parser.parsers)
            elif isinstance(parser, Defer):
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

        all_parse_fields = [
            field
            for node_type in self.astnode_types
            for field in node_type.get_parse_fields(include_inherited=False)
        ]

        for field in all_parse_fields:
            field._is_optional = False
            for parser in field.parsers_from_transform:
                if can_produce_null(parser):
                    field._is_optional = True

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

    def check_env_metadata(self, cls):
        """
        Perform legality checks on `cls`, the env metadata struct.

        :param StructType cls: Environment metadata struct type.
        """
        from langkit.compiled_types import resolve_type

        with cls.diagnostic_context:
            name = cls.dsl_name
            check_source_language(
                name == 'Metadata',
                'The environment metadata struct type must be called'
                ' "Metadata" (here: {})'.format(name)
            )

        for field in cls.get_fields():
            with field.diagnostic_context:
                typ = resolve_type(field.type)
                check_source_language(
                    typ.is_bool_type or typ.is_ast_node,
                    'Environment metadata fields can be only booleans or AST'
                    ' nodes'
                )

    def all_properties(self, *args, **kwargs):
        """
        Return an iterator on all the properties. *args and **kwargs are
        forwarded to the call to get_properties that is done on every astnode
        type.

        :rtype: seq[PropertyDef]
        """
        from langkit.compiled_types import CompiledTypeRepo
        for astnode in self.astnode_types:
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
            for over_prop in to_prop.all_overriding_properties:
                add_forward(from_prop, over_prop)

        def traverse_expr(expr):
            for ref_prop in expr.flat_subexprs(
                lambda e: isinstance(e, PropertyDef)
            ):
                add_forward(prop, ref_prop)
            for subexpr in expr.flat_subexprs():
                traverse_expr(subexpr)

        forwards: Dict[PropertyDef, Set[PropertyDef]] = {}
        backwards: Dict[PropertyDef, Set[PropertyDef]] = {}

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
        forward_map: Dict[PropertyDef, Set[PropertyDef]],
    ) -> Set[PropertyDef]:
        """
        Compute the set of properties that are transitively called (according
        to the given forward map) by a public property, Predicate parsers in
        the grammar or resolvers in env specs. Also assume that all internal
        properties are reachable.
        """
        from langkit.expressions import resolve_property
        from langkit.parsers import Predicate

        reachable_set = set()

        # First compute the set of properties called by Predicate parsers
        called_by_grammar = set()

        def visit_parser(parser):
            if isinstance(parser, Predicate):
                called_by_grammar.add(resolve_property(parser.property_ref))
            for child in parser.children:
                visit_parser(child)

        assert self.grammar is not None
        for rule in self.grammar.rules.values():
            visit_parser(rule)

        # Consider the following internal properties as "first reachables":
        #
        # * public and internal properties;
        # * properties with "warn_on_unused" disabled;
        # * properties used as entity/env resolvers.

        queue = {
            p for p in forward_map
            if p.is_public or p.is_internal or not p.warn_on_unused
        }
        queue.update(called_by_grammar)

        for astnode in self.astnode_types:
            if astnode.env_spec:
                queue.update(
                    action.resolver for action in astnode.env_spec.actions
                    if action.resolver
                )

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
            for p in prop.property_set():
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
                context_mgr = (
                    expr.abstract_expr.diagnostic_context
                    if expr.abstract_expr else
                    diagnostic_context(None)
                )

                with context_mgr:
                    check_source_language(
                        not expr.node_data.uses_entity_info
                        or expr.node_data.optional_entity_info
                        or expr.implicit_deref,
                        'Call to {} must be done on an entity'.format(
                            expr.node_data.qualname
                        ),
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
            not node._doc, 'This node lacks documentation')

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
            root = prop.root_property
            forwards[root].update(c.root_property for c in called)

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
            p.root_property for p in
            (unreachable_private_strict - unreachable_private)
        }

        def warn(unused_set, message):
            sorted_set = sorted(
                (p.qualname, p)
                for p in unused_set
                if not p.is_internal and not p.artificial
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

        for astnode in self.astnode_types:
            for prop in astnode.get_properties(include_inherited=False):
                # As we process whole properties set in one round, just focus
                # on root properties. And of course only on dispatching
                # properties.
                if prop.base_property or not prop.dispatching:
                    continue

                # Also focus on properties for which we emit code (concrete
                # ones and the ones with a runtime check).
                props = [p for p in prop.property_set()
                         if not p.abstract or p.abstract_runtime_check]

                # Set of concrete nodes that can reach this property
                nodes = set(astnode.concrete_subclasses)

                # Process properties in reverse hierarchical order to process
                # leaf properties before parent ones.
                for p in reversed(props):
                    # Compute the set of concrete subclasses that can call "p"
                    reaching_p = set(p.struct.concrete_subclasses) & nodes

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

    _template_extensions_fns: List[Callable[[CompileCtx], Dict[str, Any]]] = []
    """
    List of functions to create the default template environment.

    :type: list[(CompileCtx) -> dict[str, object]]
    """

    _template_extensions_frozen = False
    """
    Whether at least one context has requested the list of template extensions.
    Once it's true, one cannot register template extensions anymore.

    :type: bool
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
            'ada_doc':     documentation.ada_doc,
            'c_doc':       documentation.c_doc,
            'py_doc':      documentation.py_doc,
            'ocaml_doc':   documentation.ocaml_doc,
            'ada_c_doc':   documentation.ada_c_doc,
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
        exts_fn: Callable[[CompileCtx], Dict[str, Any]]
    ) -> None:
        """
        Register a set of mako template env extensions.

        :param exts_fn: Function to be evaluated the first time the renderer is
            created.
        :type exts_fn: (CompileCtx) -> dict[str, object]
        """
        assert not cls._template_extensions_frozen
        CompileCtx._template_extensions_fns.append(exts_fn)

    @staticmethod
    def load_plugin_pass(pass_or_name):
        """
        Load a plug-in pass.

        :param str|langkit.passes.AbstractPass pass_or_name: Name of the pass
            to load (``MODULE.CALLABLE`` syntax). If it is already a pass
            object, just return it.
        :rtype: langkit.passes.AbstractPass
        """
        from langkit.passes import AbstractPass

        if isinstance(pass_or_name, AbstractPass):
            return pass_or_name

        module_name, constructor_name = pass_or_name.rsplit('.', 1)
        module = importlib.import_module(module_name)
        constructor = getattr(module, constructor_name)
        result = constructor()
        assert isinstance(result, AbstractPass)
        return result

    def create_all_passes(
        self,
        lib_root: str,
        check_only: bool = False,
        warnings: Optional[WarningSet] = None,
        generate_unparser: bool = False,
        explicit_passes_triggers: Dict[str, bool] = {},
        default_max_call_depth: int = 1000,
        plugin_passes: List[Union[str, AbstractPass]] = [],
        extra_code_emission_passes: List[AbstractPass] = [],
        **kwargs
    ) -> None:
        """
        Create all the passes necessary to the compilation of the DSL. This
        should be called before ``emit``.

        :param lib_root: Path of the directory in which the library should be
            generated.

        :param check_only: If true, only perform validity checks: stop before
            code emission. This is useful for IDE hooks. False by default.

        :param warnings: If provided, white list of warnings to emit.

        :param generate_unparser: If true, generate a pretty printer for the
            given grammar. False by default.

        :param explicit_passes_triggers: Dict of optional passes names to flags
            (on/off) to trigger activation/deactivation of the passes.

        :param int max_call_depth: Default maximum number of recursive calls
            allowed in properties calls. This is used as a mitigation against
            infinite recursions.

        :param plugin_passes: List of passes to add as plugins to the
            compilation pass manager. List items must be either:

            * An instance of a``AbstractPass`` subclass.

            * A name matching the following pattern: ``MODULE.CALLABLE`` where
              ``MODULE`` is the name of a module that can be imported, and
              ``CALLABLE`` is the name of a callable inside the module to
              import. This callable must accept no argument and return an
              instance of a ``AbstractPass`` subclass.

        :param max_call_depth: Default maximum number of recursive calls
            allowed in property calls. This is used as a mitigation against
            infinite recursions.

        :param extra_code_emission_passes: See
            ``CompileCtx.code_emission_passes``.

        See ``langkit.emitter.Emitter``'s constructor for other supported
        keyword arguments.
        """

        assert self.emitter is None

        if warnings:
            self.warnings = warnings

        self.generate_unparser = generate_unparser
        self.default_max_call_depth = default_max_call_depth

        self.check_only = check_only

        if kwargs.get('coverage', False):
            self.gnatcov = GNATcov(self)

        # Load plugin passes
        loaded_plugin_passes = [self.load_plugin_pass(p)
                                for p in plugin_passes]

        # Compute the list of passes to run:

        # First compile the DSL
        self.all_passes = self.compilation_passes

        # Then, if requested, emit code for the generated library
        if not self.check_only:
            self.all_passes.append(
                self.prepare_code_emission_pass(lib_root, **kwargs))

            self.all_passes.extend(
                self.code_emission_passes(extra_code_emission_passes)
            )

            # Run plugin passes at the end of the pipeline
            self.all_passes.extend(loaded_plugin_passes)

        for p in self.all_passes:
            if p.is_optional and p.name in explicit_passes_triggers.keys():
                trig = explicit_passes_triggers.pop(p.name)
                p.disabled = not(trig)

        for n in explicit_passes_triggers.keys():
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

        if self.lexer is None:
            from langkit.lkt_lowering import create_lexer
            self.lexer = create_lexer(self, self.lkt_units)

        if self.grammar is None:
            from langkit.lkt_lowering import create_grammar
            self.grammar = create_grammar(self, self.lkt_units)

        if self.types_from_lkt and self.lkt_units:
            from langkit.lkt_lowering import create_types
            create_types(self, self.lkt_units)

    def check_lkt(self) -> None:
        """
        Run checks on the lkt sources.
        """
        errors = False

        # First check the absence of syntax errors in all loaded units
        for unit in self.lkt_units:
            if unit.diagnostics:
                for diag in unit.diagnostics:
                    errors = True
                    print_error(
                        diag.message,
                        Location.from_sloc_range(unit, diag.sloc_range)
                    )

        # Then check for semantic errors either because requested
        # (self.lkt_semantic_checks) or because everything is loaded from Lkt
        # sources.
        #
        # NOTE: we cannot automatically enable semantic checks when
        # types_from_lkt is false, as in this case Liblktlang may not be able
        # to handle some property DSL feature.
        #
        # NOTE: for the moment let's not even try to analyze anything if we
        # have syntax errors.
        if not errors and (self.lkt_semantic_checks or self.types_from_lkt):
            for unit in self.lkt_units:
                sem_results = cast(L.LangkitRoot, unit.root).p_check_semantic
                errors = errors or sem_results.has_error
                for r in sem_results.results:
                    if r.error_message:
                        print_error_from_sem_result(r)

    def prepare_compilation(self):
        """
        Prepare this context to compile the DSL.

        TODO: this pass seems like a weird grab bag of verifications and a
        potentially useless assignment. See if it can be removed later.
        """
        from langkit.compiled_types import CompiledTypeRepo

        # Compilation cannot happen more than once
        assert not self.compiled

        # Make sure user provided a grammar
        assert self.grammar, 'Set grammar before compiling'

        self.root_grammar_class = CompiledTypeRepo.root_grammar_class

        if self.generate_unparser:
            self.warnings.enable(self.warnings.unparser_bad_grammar)

    def prepare_code_emission_pass(self, lib_root, **kwargs):
        """
        Return a pass to prepare this context for code emission.
        """
        from langkit.emitter import Emitter
        from langkit.passes import GlobalPass

        def pass_fn(ctx):
            ctx.emitter = Emitter(
                self,
                lib_root,
                ctx.extensions_dir,
                post_process_ada=self.post_process_ada,
                post_process_cpp=self.post_process_cpp,
                post_process_python=self.post_process_python,
                post_process_ocaml=self.post_process_ocaml,
                **kwargs
            )

        return GlobalPass('prepare code emission', pass_fn)

    def compile(self, generate_unparser=False):
        """
        Compile the DSL.

        :param bool generate_unparser: If true, generate a pretty printer for
            the given grammar. False by default.
        """
        self.generate_unparser = generate_unparser

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
    def iterator_types(self) -> List[IteratorType]:
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
        from langkit.compiled_types import CompiledTypeRepo

        enum_types = CompiledTypeRepo.enum_types

        if self._enum_types:
            assert len(self._enum_types) == len(enum_types), (
                'CompileCtx.enum_types called too early: more enum types were'
                ' added')
        else:
            self._enum_types = list(enum_types)
            enum_types.sort(key=lambda et: et.name)

        return self._enum_types

    @property
    def compilation_passes(self):
        """
        Return the list of passes to compile the DSL.
        """
        from langkit.envs import EnvSpec
        from langkit.expressions import PropertyDef
        from langkit.lexer import Lexer
        from langkit.parsers import Grammar, Parser
        from langkit.passes import (
            ASTNodePass, EnvSpecPass, GlobalPass, GrammarPass, GrammarRulePass,
            LexerPass, MajorStepPass, PropertyPass, errors_checkpoint_pass
        )

        # RA22-015: in order to allow bootstrap, we need to import liblktlang
        # only if we are about to process LKT grammar rules.
        def lower_grammar_rules(ctx):
            if not ctx.grammar._all_lkt_rules:
                return
            from langkit.lkt_lowering import lower_grammar_rules
            lower_grammar_rules(ctx)

        return [
            MajorStepPass('Lkt processing'),
            GlobalPass('Lkt semantic analysis', CompileCtx.check_lkt),
            errors_checkpoint_pass,
            GlobalPass('lower Lkt', CompileCtx.lower_lkt),
            GlobalPass('prepare compilation', CompileCtx.prepare_compilation),

            MajorStepPass('Compiling the lexer'),
            LexerPass('check token families', Lexer.check_token_families),
            LexerPass('compile lexer rules', Lexer.compile_rules),

            MajorStepPass('Compiling the grammar'),
            GlobalPass('lower Lkt parsing rules', lower_grammar_rules),
            GrammarPass('check main parsing rule', Grammar.check_main_rule),
            GrammarPass('warn on unreferenced parsing rules',
                        Grammar.warn_unreferenced_parsing_rules),
            EnvSpecPass('create internal properties for env specs',
                        EnvSpec.create_properties,
                        iter_metaclass=True),
            EnvSpecPass('register categories', EnvSpec.register_categories,
                        iter_metaclass=True),
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
            GlobalPass('compute optional field info',
                       CompileCtx.compute_optional_field_info),
            ASTNodePass('reject abstract AST nodes with no concrete'
                        ' subclasses', CompileCtx.check_concrete_subclasses),
            errors_checkpoint_pass,

            MajorStepPass('Compiling properties'),
            PropertyPass('compute base properties',
                         PropertyDef.compute_base_property),
            PropertyPass('prepare abstract expressions',
                         PropertyDef.prepare_abstract_expression),
            PropertyPass('freeze abstract expressions',
                         PropertyDef.freeze_abstract_expression),
            PropertyPass('compute property attributes',
                         PropertyDef.compute_property_attributes),
            PropertyPass('construct and type expressions',
                         PropertyDef.construct_and_type_expression),
            PropertyPass('check overriding types',
                         PropertyDef.check_overriding_types),
            PropertyPass('check properties returning node types',
                         PropertyDef.check_returned_nodes),
            GlobalPass('compute properties callgraphs',
                       CompileCtx.compute_properties_callgraphs),
            GlobalPass('compute uses entity info attribute',
                       CompileCtx.compute_uses_entity_info_attr),
            GlobalPass('compute uses envs attribute',
                       CompileCtx.compute_uses_envs_attr),
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
            GlobalPass('check memoized properties',
                       CompileCtx.check_memoized),
            GlobalPass('lower properties dispatching',
                       CompileCtx.lower_properties_dispatching),
            GlobalPass('compute AST node constants',
                       CompileCtx.compute_astnode_constants),
            errors_checkpoint_pass,

            MajorStepPass('Computing precise types'),
            ASTNodePass('compute precise fields types',
                        lambda _, n: n.compute_precise_fields_types()),

            GrammarRulePass('compile parsers', Parser.compile),
            GrammarRulePass('compute nodes parsers correspondence',
                            self.unparsers.compute),
            ASTNodePass('warn imprecise field type annotations',
                        lambda _, astnode:
                        astnode.warn_imprecise_field_type_annotations()),
            GlobalPass('log node parsers correspondence',
                       self.unparsers.check_nodes_to_rules),
            GlobalPass('finalize unparsers code generation',
                       self.unparsers.finalize),
        ]

    def code_emission_passes(
        self, extra_passes: List[AbstractPass]
    ) -> List[AbstractPass]:
        """
        Return the list of passes to emit sources for the generated library.

        :param extra_passes: List of passes to run before generating right
            before the library project file. This allows manage scripts to
            generate extra Ada sources.
        """
        from langkit.emitter import Emitter
        from langkit.expressions import PropertyDef
        from langkit.parsers import Parser
        from langkit.passes import (
            EmitterPass, GlobalPass, GrammarRulePass, MajorStepPass,
            PropertyPass, errors_checkpoint_pass
        )

        from langkit.dsl_unparse import unparse_lang
        from langkit.railroad_diagrams import emit_railroad_diagram

        return [
            MajorStepPass('Prepare code emission'),

            GrammarRulePass('register parsers symbol literals',
                            Parser.add_symbol_literals),
            # Past this point, the set of symbol literals is frozen
            GlobalPass('finalize symbol literals',
                       CompileCtx.finalize_symbol_literals),

            GrammarRulePass('render parsers code',
                            lambda p: Parser.render_parser(p, self)),
            PropertyPass('render property', PropertyDef.render_property),
            GlobalPass('annotate fields types',
                       CompileCtx.annotate_fields_types).optional(
                """
                Auto annotate the type of fields in your nodes definitions,
                based on information derived from the grammar.
                """
            ),
            errors_checkpoint_pass,

            MajorStepPass('Generate library sources'),
            EmitterPass('setup directories', Emitter.setup_directories),
            EmitterPass('generate lexer DFA', Emitter.generate_lexer_dfa),
            EmitterPass('emit Ada sources', Emitter.emit_ada_lib),
            EmitterPass('emit mains', Emitter.emit_mains),
            EmitterPass('emit C API', Emitter.emit_c_api),
            EmitterPass('emit Python API', Emitter.emit_python_api),
            EmitterPass('emit Python playground',
                        Emitter.emit_python_playground),
            EmitterPass('emit GDB helpers', Emitter.emit_gdb_helpers),
            EmitterPass('emit OCaml API', Emitter.emit_ocaml_api),
        ] + extra_passes + [
            EmitterPass('emit library project file',
                        Emitter.emit_lib_project_file),
            EmitterPass('instrument for code coverage',
                        Emitter.instrument_for_coverage),

            GrammarRulePass('emit railroad diagrams', emit_railroad_diagram)
            .optional("""
            Emit SVG railroad diagrams for grammar rules, in share/doc. Needs
            the railroad-diagrams Python library.
            """),

            GlobalPass('report unused documentation entries',
                       lambda ctx: ctx.documentations.report_unused())
            .optional(
                """
                Report unused documentation entries. This is an internal pass
                that is used by Langkit devs to maintain the langkit
                documentation.
                """
            ),

            GlobalPass('RA22-015: Unparse language to concrete syntax',
                       unparse_lang),
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

    @extensions_dir.setter
    def extensions_dir(self, ext_dir):
        # Only set the extensions dir if this directory exists
        if os.path.isdir(ext_dir):
            self._extensions_dir = os.path.abspath(ext_dir)

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
            candidate_name = (names.Name('Precomputed_Sym') +
                              names.Name.from_lower(tmp_2.strip('_')))

            # If the candidate is already used, add an unique number
            if candidate_name in self.symbol_literals.values():
                candidate_name = candidate_name + names.Name(str(i))

            self.symbol_literals[name] = candidate_name

    def annotate_fields_types(self):
        """
        Modify the Python files where the node types are defined, to annotate
        empty Field() definitions.
        """
        # Only import lib2to3 if the users needs it
        import lib2to3.main

        astnodes_files = {
            n.location.file
            for n in self.astnode_types
            if n.location is not None
        }

        lib2to3.main.main(
            "langkit",
            ["-f", "annotate_fields_types",
             "--no-diff", "-w"] + list(astnodes_files)
        )

    def compute_astnode_constants(self):
        """
        Compute several constants for the current set of AST nodes.
        """
        # Compute the set of "kind" constants
        for i, astnode in enumerate(
            (astnode
             for astnode in self.astnode_types
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
        for n in self.astnode_types:
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
                if (f.abstract or not f.overriding) and f.struct is n:
                    self.sorted_parse_fields.append(f)

            for p in n.get_properties(predicate=lambda p: p.is_public,
                                      include_inherited=False):
                if not p.overriding:
                    self.sorted_properties.append(p)

    def compute_composite_types(self):
        """
        Check that struct and array types are valid and compute related lists.

        Today this only checks that there is no inclusing loop between these
        types. For instance: (1) is an array of (2) and (2) is a struct that
        contains (1).
        """
        from langkit.compiled_types import CompiledTypeRepo, T

        def dependencies(typ):
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
        struct_types = CompiledTypeRepo.struct_types
        array_types = CompiledTypeRepo.array_types
        iterator_types = set(CompiledTypeRepo.iterator_types)
        CompiledTypeRepo.struct_types = None
        CompiledTypeRepo.array_types = None
        CompiledTypeRepo.iterator_types = None

        # To avoid generating too much bloat, we generate C API interfacing
        # code only for iterators on root entities.  Bindings for iterators on
        # all other entity types can re-use this code, as all entity types have
        # the same ABI. This means we expose iterator for root entities as soon
        # as an iterator on any entity type is exposed.
        if any(t.element_type.is_entity_type for t in iterator_types):
            iterator_types.add(T.entity.iterator)

        # Sort the composite types by dependency order
        types_and_deps = (
            [(st, dependencies(st)) for st in struct_types]
            + [(at, dependencies(at)) for at in array_types]
            + [(it, dependencies(it)) for it in iterator_types])
        try:
            self._composite_types = topological_sort(types_and_deps)
        except TopologicalSortError as exc:
            message = ['Invalid composition of types:']
            for i, item in enumerate(exc.loop):
                next_item = (exc.loop[i + 1]
                             if i + 1 < len(exc.loop) else
                             exc.loop[0])
                message.append('  * {} contains a {}'
                               .format(item.dsl_name, next_item.dsl_name))
            check_source_language(False, '\n'.join(message))

        self._array_types = [t for t in self._composite_types
                             if t.is_array_type]
        self._iterator_types = [t for t in self._composite_types
                                if t.is_iterator_type]
        self._struct_types = [t for t in self._composite_types
                              if t.is_struct_type]
        self._entity_types = [t for t in self._composite_types
                              if t.is_entity_type]

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
                for dv in f.dynamic_vars:
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

    def lower_properties_dispatching(self):
        """
        Lower all dispatching properties.

        For each set of related dispatching properties, create a wrapper one
        that does manual dispatching based on AST node kinds and make all the
        other ones non-dispatching and private.
        """
        from langkit.compiled_types import Argument
        from langkit.expressions import (
            FieldAccess, PropertyDef, ResolvedExpression, Super,
        )

        # This pass rewrites properties, so it invalidates callgraphs
        self.properties_forwards_callgraphs = None
        self.properties_backwards_callgraphs = None

        redirected_props = {}

        # Iterate on AST nodes in hierarchical order, so that we meet root
        # properties before the overriding ones. As processing root properties
        # will remove the dispatching attribute of all overriding ones, we will
        # not process the same property twice.
        for astnode in self.astnode_types:
            for prop in astnode.get_properties(
                lambda p: p.dispatching,
                include_inherited=False
            ):
                # `prop` must be the ultimate base property: see the above
                # comment.
                prop_set = prop.property_set()
                assert prop_set[0] == prop

                static_props = list(prop_set)
                static_props.sort(key=lambda p: p.struct.hierarchical_name)

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
                prop_name = prop.name
                prop._name = names.Name('Dispatcher') + prop.name

                if not prop.abstract or prop.abstract_runtime_check:
                    root_static = PropertyDef(
                        expr=None, prefix=None, name=None,
                        type=prop.type,
                        doc=prop.doc,
                        public=False,
                        dynamic_vars=prop.dynamic_vars,
                        uses_entity_info=prop.uses_entity_info,
                        uses_envs=prop.uses_envs,
                        optional_entity_info=prop.optional_entity_info,
                        lazy_field=prop.lazy_field,
                    )
                    static_props[0] = root_static

                    # Add this new property to its structure for code
                    # generation. Make sure it is registered under a name that
                    # is different from the dispatcher so that both are present
                    # in the structures' field dict.
                    root_static._name = prop_name
                    root_static._original_name = prop._original_name
                    root_static._indexing_name = (
                        '[root-static]{}'.format(prop.indexing_name)
                    )
                    prop.struct.add_field(root_static)

                    # Transfer arguments from the dispatcher to the new static
                    # property, then regenerate arguments in the dispatcher.
                    root_static.arguments = prop.arguments
                    prop.arguments = [
                        Argument(arg.name, arg.type, arg.is_artificial,
                                 arg.abstract_default_value)
                        for arg in prop.natural_arguments
                    ]
                    prop.build_dynamic_var_arguments()

                    root_static.constructed_expr = prop.constructed_expr
                    prop.expected_type = prop.type
                    prop.constructed_expr = None

                    root_static.vars = prop.vars
                    prop.vars = None

                    root_static.abstract_runtime_check = (
                        prop.abstract_runtime_check)
                    prop.abstract_runtime_check = False

                    root_static._has_self_entity = prop._has_self_entity

                    root_static.struct = prop.struct
                    root_static.location = prop.location
                    prop.is_artificial_dispatcher = True

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
                            and isinstance(expr.abstract_expr, Super)
                            and expr.node_data == prop
                        ):
                            expr.node_data = root_static

                        for subexpr in expr.flat_subexprs(
                            lambda e: isinstance(e, ResolvedExpression)
                        ):
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
                    prop.struct, reversed([p.struct for p in static_props]))
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
        for astnode in self.astnode_types:
            if astnode.env_spec:
                for env_action in astnode.env_spec.actions:
                    env_action.rewrite_property_refs(redirected_props)

    def generate_actions_for_hierarchy(self, node_var, kind_var,
                                       actions_for_astnode,
                                       public_nodes=False):
        """
        Generate a sequence of Ada statements/nested CASE blocks to execute
        some actions on an AST node depending on its kind.

        This method is useful to avoid generating the same statements over and
        over for multiple AST node kinds. For instance, given a root AST node
        `A` a field `f` on `A`, and its derivations `B`, `C` and `D`, if one
        wants to perform clean-up on some AST node fields, there is no need to
        generate specific code for `B.f`, `C.f` and `D.f` when we could just
        generate code for `A.f`.

        :param str|None node_var: Name of the variable that holds the AST node
            to process. None if the generated code must work only on the kind.
        :param str kind_var: Name of the variable that holds the kind of the
            AST node to process. Holding it in a variables is handy to avoid
            computing it multiple times.
        :param (ASTNodeType, str) -> str actions_for_astnode: Function that
            takes a specific node type to process and the name of the variable
            to hold the node instance to process. It must return the actions
            (i.e. Ada statements as a single string) to perform for this node.
            Note that these actions should be specific to the node type, i.e.
            they should not overlap with actions for any parent node.
        :type actions_for_astnode: (ASTNodeType) -> str
        :param bool public_nodes: Whether `node_var` is a type from the public
            API. Otherwise (the default), assume it is an internal node.
        """

        class Matcher:
            """
            Holder for "when ... =>" clauses in a CASE block.
            """

            def __init__(self, astnode, actions):
                self.astnode = astnode
                """
                AST node that `self` matches.
                :type: ASTNodeType
                """

                self.actions = actions
                """
                List of actions specific to this matched AST node.
                :type: str
                """

                self.inner_case = Case(astnode)
                """
                Case instance for nodes that are more specific than `astnode`.
                :type: Case
                """

            @staticmethod
            def new_node_var(astnode):
                """
                Return the variable name that will hold the casted value for
                the matched AST node.

                :rtype: names.Name
                """
                return names.Name('N') + astnode.name

        class Case:
            """
            Holder for a generated CASE blocks.
            """

            def __init__(self, astnode):
                self.astnode = astnode
                """
                Most specific type for this CASE block's input expression.
                :type: ASTNodeType
                """

                self.matchers = []
                """
                List of matchers for this CASE block.
                :type: list[Matcher]
                """

        root_node = self.root_grammar_class

        result = []
        """
        List of strings for the sequence of Ada statements to return.
        :type: list[str]
        """

        case_stack = [Case(root_node)]
        """
        Stack of Case instances for the Case tree we are currently building.
        First element is for the top-level CASE node while the last element is
        for the currently inner-most CASE node.
        :type: list[Case]
        """

        def build_cases(astnode):
            """
            Build the tree of CASE blocks for `astnode` and all its subclasses.
            """
            # Don't bother processing classes unless they actually have
            # concrete subclasses, otherwise we would be producing dead code.
            if not astnode.concrete_subclasses:
                return

            to_pop = False

            if astnode == root_node:
                # As a special case, emit actions for the root node outside of
                # the top-level CASE block as we don't need to dispatch on
                # anything for them: they always must be applied.
                actions = actions_for_astnode(astnode, node_var)
                if actions:
                    result.append(actions)

            else:
                # If there are actions for this node, add a matcher for them
                # and process the subclasses in a nested CASE block.
                actions = actions_for_astnode(
                    astnode, Matcher.new_node_var(astnode)
                )
                if actions:
                    m = Matcher(astnode, actions)
                    case_stack[-1].matchers.append(m)
                    case_stack.append(m.inner_case)
                    to_pop = True

            for subcls in astnode.subclasses:
                build_cases(subcls)

            if to_pop:
                case_stack.pop()

        def print_case(case, node_var):
            """
            Render a tree of CASE blocks and append them to `result`.

            :param Case case: CASE block to render.
            :param str node_var: Name of the variable that holds the node on
                which this CASE must dispatch.
            """
            if not case.matchers:
                return

            result.append('case {} ({}) is'.format(
                case.astnode.ada_kind_range_name, kind_var
            ))
            for m in case.matchers:
                result.append(
                    'when {} =>'.format(m.astnode.ada_kind_range_name)
                )
                if node_var is None:
                    new_node_var = None
                else:
                    new_node_type = (
                        m.astnode.entity.api_name.camel_with_underscores
                        if public_nodes else
                        m.astnode.name.camel_with_underscores)
                    new_node_var = m.new_node_var(m.astnode)

                    # Declare a new variable to hold the node subtype to
                    # process in this matcher.
                    new_node_expr = ('{node_var}.As_{new_node_type}'
                                     if public_nodes else
                                     str(node_var))
                    result.append('declare')
                    result.append(
                        ('{new_node_var} : constant'
                         ' {namespace}{new_node_type} := ' +
                         new_node_expr + ';').format(
                            node_var=node_var,
                            new_node_type=new_node_type,
                            new_node_var=new_node_var,

                            # Public node names sometimes clash with
                            # introspection enumerations. Adding namespace
                            # helps generating correct code.
                            namespace='Analysis.' if public_nodes else ''))
                    result.append('begin')

                result.append(m.actions)
                print_case(m.inner_case, new_node_var)
                if node_var is not None:
                    result.append('end;')

            result.append('when others => null;')
            result.append('end case;')

        with names.camel_with_underscores:
            build_cases(root_node)
            assert len(case_stack) == 1
            print_case(case_stack[0], node_var)

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
        for astnode in self.astnode_types:
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
                    prop.struct.add_as_memoization_key(self)
                    if prop.uses_entity_info:
                        T.entity_info.add_as_memoization_key(self)
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

            with prop.diagnostic_context:
                check_source_language(False, message,
                                      severity=Severity.non_blocking_error)

    TypeSet = utils.TypeSet

    astnode_kind_set = utils.astnode_kind_set

    collapse_concrete_nodes = staticmethod(
        utils.collapse_concrete_nodes
    )
