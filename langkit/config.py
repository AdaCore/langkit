"""
Logic to represent and load configuration for a language specification.
"""

from __future__ import annotations

import argparse
import dataclasses
import os.path
from typing import Literal, NoReturn, Protocol, Self, TypeVar

from langkit.diagnostics import Location, WarningSet, diagnostic_context, error
import langkit.names as names


#
# JSON decoding helpers
#


def json_sub_context(context: str, name: str) -> str:
    """
    Return a sub-context.
    """
    if ":" in name:
        name = repr(name)
    return f"{context}:{name}"


def json_error(context: str, message: str) -> NoReturn:
    """
    Format and propagate a diagnostic error for the given configuration context
    and the given message.
    """
    error(f"{context}: {message}")


def json_type_error(context: str, value: object, exp_type: str) -> NoReturn:
    """
    Specialization of ``json_error`` to reject an invalid value type found in
    the configuration.

    :param context: Context where the invalid value was found.
    :param value: Invalid value that was found.
    :param exp_type: Human-readable description of the type that was expected.
    """
    json_error(context, f"{exp_type} expected, got a {type(value).__name__}")


T = TypeVar("T", covariant=True)


class JSONDecoder(Protocol[T]):
    """
    Callback used to decode a JSON value.
    """
    def __call__(self, context: str, json: object) -> T:
        """
        :param context: Context where the value was found.
        :param json: JSON value to decode.
        :return: The decoded value. In case of error, a diagnostic error must
            be propagated (see ``json_error`` or ``json_type_error``).
        """
        ...


def json_boolean(context: str, json: object) -> bool:
    """
    JSON value decoder when a boolean is expected.
    """
    if isinstance(json, bool):
        return json
    else:
        json_type_error(context, json, "boolean")


def json_integer(context: str, json: object) -> int:
    """
    JSON value decoder when an integer is expected.
    """
    if isinstance(json, int):
        return json
    else:
        json_type_error(context, json, "integer")


def json_string(context: str, json: object) -> str:
    """
    JSON value decoder when a string is expected.
    """
    if isinstance(json, str):
        return json
    else:
        json_type_error(context, json, "string")


def json_list(decoder: JSONDecoder[T]) -> JSONDecoder[list[T]]:
    """
    Return a JSON value decoder for when a list is expected.

    :param decoder: JSON value decoder for list elements.
    """
    def helper(context: str, json: object) -> list[T]:
        if isinstance(json, list):
            result: list[T] = []
            for i, item in enumerate(json):
                result.append(decoder(f"{context}[{i}]", item))
            return result
        else:
            json_type_error(context, json, "array")

    return helper


def json_dict(decoder: JSONDecoder[T]) -> JSONDecoder[dict[str, T]]:
    """
    Return a JSON value decoder for when a dict is expected.

    Note that dicts are always expected to be indexed by strings.

    :param decoder: JSON value decoder for dict values.
    """
    def helper(context: str, json: object) -> dict[str, T]:
        if isinstance(json, dict):
            result: dict[str, T] = {}
            for name, value in json.items():
                result[name] = decoder(json_sub_context(context, name), value)
            return result
        else:
            json_type_error(context, json, "dict")

    return helper


def json_name(context: str, json: object) -> names.Name:
    """
    JSON value decoder when a name is expected.

    Names are expected to use the "camel_with_underscores" naming convention.
    """
    if isinstance(json, str):
        try:
            return names.Name(json)
        except ValueError as exc:
            json_error(context, str(exc))
    else:
        json_type_error(context, json, "name")


class MISSING:
    """
    Singleton used as a result for JSONDictDecodingContext.pop_optional to
    mean: no value found.
    """
    pass


class JSONDictDecodingContext:
    """
    Helper to decode JSON dicts.

    Instances are meant to be used as context managers, and "pop" operations
    used inside the context block::

       with JSONDictDecodingContext(context, value) as d:
          foo = d.pop("foo", json_boolean)
          bar = d.pop_optional("bar", json_integer)
          ...

    On context block exit, this checks that all dict entries in ``value`` were
    processed, and propagates a diagnostic error if this is not the case, so
    that extra entries added by mistake are not ignored.
    """

    def __init__(self, context: str, json: object):
        """
        :param context: Context where this dict was found.
        :param json: Dict value to decode. This may actually not be a dict: in
            this case, a diagnostic error is propagated.
        """
        if not isinstance(json, dict):
            json_type_error(context, json, "dict")

        self.context = context

        # The "pop" operations below mutate the dict, so create a copy to leave
        # the original dict unchanged.
        self.values = dict(json)

    def sub_context(self, name: str) -> str:
        """
        Return the context for a dict entry.

        :param name: Name for the dict entry (i.e. a dict key).
        """
        return json_sub_context(self.context, name)

    def pop_optional(self, name: str, decoder: JSONDecoder[T]) -> T | MISSING:
        """
        Look for an optional dict entry and return it in its decoded form. If
        not found, return a ``MISSING`` instance.

        :param name: Name of the dict entry to lookup.
        :param decoder: JSON decoder for the dict entry value.
        """
        try:
            value = self.values.pop(name)
        except KeyError:
            return MISSING()
        else:
            return decoder(self.sub_context(name), value)

    def pop(self, name: str, decoder: JSONDecoder[T]) -> T:
        """
        Look for a mandatory dict entry and return it in its decoded form.

        :param name: Name of the dict entry to lookup.
        :param decoder: JSON decoder for the dict entry value.
        """
        result = self.pop_optional(name, decoder)
        if isinstance(result, MISSING):
            json_error(self.context, f"missing {name!r} entry")
        else:
            return result

    def __enter__(self) -> Self:
        return self

    def __exit__(
        self,
        exc_type: type[Exception] | None,
        exc_val: Exception | None,
        exc_tb: object,
    ) -> Literal[False]:
        if exc_type is None and self.values:
            json_error(
                self.context,
                "unknown entries: {}".format(", ".join(sorted(self.values)))
            )
        return False


#
# Configuration data structures
#


@dataclasses.dataclass(frozen=True)
class LibraryEntity:
    """
    Reference to an entity in the generated library.
    """

    unit_fqn: str
    """
    Fully qualified name for the unit that contains the referenced entity. For
    instance: "Libfoolang.My_Unit".
    """
    entity_name: str
    """
    Simple name for the entity that is referenced.
    """

    @property
    def fqn(self) -> str:
        """
        Fully qualified name for the referenced entity.

        For instance: "Libfoolang.My_Unit.My_Entity".
        """
        return '{}.{}'.format(self.unit_fqn, self.entity_name)

    @classmethod
    def from_fqn(cls, value: str) -> LibraryEntity:
        """
        Create a library entity from its fully qualified name.
        """
        if "." not in value:
            raise ValueError(
                f"invalid library entity: {value!r}, fully qualified name"
                " expected"
            )
        unit, entity = value.rsplit(".", 1)
        return cls(unit, entity)

    @classmethod
    def from_json(cls, context: str, json: object) -> LibraryEntity:
        match json:
            case str(json):
                try:
                    return cls.from_fqn(json)
                except ValueError as exc:
                    json_error(context, str(exc))
            case _:
                json_type_error(context, json, "string")


@dataclasses.dataclass
class LibraryDefaults:
    """
    Default values for runtime settings in the generated library.
    """

    charset: str = "utf-8"
    """
    In the generated library, this will be the default charset to use to scan
    input source files.
    """

    tab_stop: int = 8
    """
    Tabulation stop to use as a default value in the analysis context
    constructor.
    """

    unit_provider: LibraryEntity | None = None
    """
    If provided, it must denote a
    Langkit_Support.Unit_Files.Unit_Provider_Reference object (or argument-less
    function) in the generated library. This object will be used as the default
    unit provider during the creation of an analysis context.

    If None, this disables altogether the unit provider mechanism in code
    generation.
    """

    unparsing_config: str | None = None
    """
    Filename relative to the extensions directory, containing the default JSON
    unparsing configuration for the generated library. Use an empty
    configuration if omitted.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> LibraryDefaults:
        with JSONDictDecodingContext(context, json) as d:
            result = cls()

            match d.pop_optional("charset", json_string):
                case str(charset):
                    result.charset = charset

            match d.pop_optional("tab_stop", json_integer):
                case int(tab_stop):
                    result.tab_stop = tab_stop

            match d.pop_optional("unit_provider", LibraryEntity.from_json):
                case (LibraryEntity() | None) as up:
                    result.unit_provider = up

            match d.pop_optional("unparsing_config", json_string):
                case str(filename):
                    result.unparsing_config = filename

        return result


@dataclasses.dataclass(frozen=True)
class ExternalException:
    """
    Description for an Ada exception that is not built in Langkit (i.e.
    declared in a dependency, or in extension code), and which may be
    propagated to callers in public APIs.
    """

    exception: LibraryEntity
    """
    Reference to the Ada exception declaration.
    """

    doc_section: str
    """
    Name of the doc section that documents this exception (i.e. where to look
    in extra documentations to document this exception in generated code).
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> ExternalException:
        with JSONDictDecodingContext(context, json) as d:
            return cls(
                exception=d.pop("exception", LibraryEntity.from_json),
                doc_section=d.pop("doc_section", json_string),
            )


@dataclasses.dataclass
class ContextClause:
    """
    Description of a context clause to add to a generated unit.
    """

    with_unit: str
    """
    Name of the target unit for the context clause.
    """

    use: bool = False
    """
    Whether to generate a "use" clause for this unit.
    """

    private: bool = False
    """
    Whether the "with" clause to generate for this unit should have the
    "private" keyword.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> ContextClause:
        with JSONDictDecodingContext(context, json) as d:
            result = cls(with_unit=d.pop("with", json_string))

            match d.pop_optional("use", json_boolean):
                case bool(use):
                    result.use = use

            match d.pop_optional("private", json_boolean):
                case bool(private):
                    result.private = private

            return result


@dataclasses.dataclass
class UnitContextClauses:
    """
    Context clauses for the two parts of a generated unit.
    """

    spec: list[ContextClause] = dataclasses.field(default_factory=list)
    """
    Context clauses for this unit's spec.
    """

    body: list[ContextClause] = dataclasses.field(default_factory=list)
    """
    Context clauses for this unit's body.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> UnitContextClauses:
        with JSONDictDecodingContext(context, json) as d:
            result = cls()

            match d.pop_optional("spec", json_list(ContextClause.from_json)):
                case list() as spec:
                    result.spec = spec

            match d.pop_optional("body", json_list(ContextClause.from_json)):
                case list() as body:
                    result.body = body

            return result


@dataclasses.dataclass
class CacheCollectionConfig:
    """
    Describes a strategy to use for automatic cache collection.
    """

    threshold_increment: int
    """
    Indicates the number of additional lexical env cache entries needed after
    the last cache collection to trigger a new one. To give an example,
    assume that we reached the current threshold set to 1000 entries. Hence, a
    collection is attempted. If after this collection the number of entries is
    back to, say 800, then we add to it this ``threshold_increment``, say 200,
    to get the new threshold. This means a new collection will be attempted
    when we reach 1000 entries again. Must be positive.
    """

    decision_heuristic: LibraryEntity | None = None
    """
    The heuristic to use to decide whether a given unit should have its lexical
    env caches collected or not. When left to ``None``, this will use the
    default heuristic which will end up clearing the caches of all units when
    the collection threshold is reached.
    """

    def __post_init__(self) -> None:
        assert self.threshold_increment > 0

    @classmethod
    def from_json(cls, context: str, json: object) -> CacheCollectionConfig:
        with JSONDictDecodingContext(context, json) as d:
            result = cls(
                threshold_increment=d.pop("threshold_increment", json_integer)
            )

            match d.pop_optional(
                "decision_heuristic", LibraryEntity.from_json
            ):
                case LibraryEntity() as heuristic:
                    result.decision_heuristic = heuristic

        return result


@dataclasses.dataclass
class LibraryConfig:
    """
    Configuration for the library to compile.
    """

    root_directory: str
    """
    Root directory for the language spec.
    """

    language_name: names.Name
    """
    Name for the language to analyze.
    """

    library_name: names.Name | None = None
    """
    If provided, set the name for the library to generate. Otherwise set to
    "Lib<language_name>lang". It is used for the filenames, package names, etc.
    in the generated library.
    """

    short_name: str | None = None
    """
    If provided, short name for the library to generate (lower case).  It will
    be used where a short name for the library is requested, for instance for
    the shortcut module name in the generated "playground" script.
    """

    version: str = "undefined"
    """
    String for the version of the generated library.
    """

    build_date: str = "undefined"
    """
    String for the generated library build date (where "build" includes source
    generation).
    """

    standalone: bool = False
    """
    Whether to generate a library that does not depend on Langkit_Support (it
    will still depend on LibGPR and GNATCOLL).

    Note that since several units from Langkit_Support are used in public APIs,
    the API of a standalone library is incompatible with the API of the
    equivalent regular library.

    Because of this, standalone libraries should be used only as an internal
    implementation helper in a bigger library, and units of the former should
    not appear in the public API of the latter.
    """

    defaults: LibraryDefaults = dataclasses.field(
        default_factory=LibraryDefaults
    )

    symbol_canonicalizer: LibraryEntity | None = None
    """
    If provided, it must denote a subprogram in the generated library to call
    in order to canonicazie symbol identifiers. Such a suprogram must have the
    following signature::

       function Canonicalize
         (Name : Text_Type) return Symbolization_Result;

    It takes an identifier name and must return the canonical name for it (or
    an error), so that all equivalent symbols have the same canonical name.

    This can be used, for instance, to implement case insensivity, but if case
    insensitivity is all you need, prefer the @case_insensitive lexer
    annotation, which provides a default symbol canonicalizer.
    """

    external_exceptions: list[ExternalException] = dataclasses.field(
        default_factory=list
    )
    """
    List of Ada exceptions that are declared outside of the generated library.
    """

    property_exceptions: list[str] = dataclasses.field(default_factory=list)
    """
    In addition to ``Property_Error``, set of names for exceptions that
    properties are allowed to raise.
    """

    extra_context_clauses: dict[str, UnitContextClauses] = dataclasses.field(
        default_factory=dict
    )
    """
    Extra context clauses for generated Ada units.

    Keys designate fully qualified names (without the library name prefix: e.g.
    Analysis, Implemenattion.C, ...) of units that need extra context clauses.
    """

    cache_collection: CacheCollectionConfig | None = None
    """
    If not None, setup the automatic cache collection mechanism with this
    configuration.
    """

    extra_install_files: dict[str, list[str]] = dataclasses.field(
        default_factory=dict
    )
    """
    Extra files to install when installing the library.

    This maps destination directories (relative to the installation prefix) to
    the files/directories to install (relative to the library root directory).
    """

    @property
    def actual_library_name(self) -> names.Name:
        return (
            names.Name('Lib{}lang'.format(self.language_name.lower))
            if self.library_name is None else
            self.language_name
        )

    @classmethod
    def from_json(cls, base_directory: str) -> JSONDecoder[LibraryConfig]:
        def wrapper(context: str, json: object) -> LibraryConfig:
            return cls._from_json(base_directory, context, json)
        return wrapper

    @classmethod
    def _from_json(
        cls,
        base_directory: str,
        context: str,
        json: object
    ) -> LibraryConfig:
        with JSONDictDecodingContext(context, json) as d:
            result = cls(
                root_directory=base_directory,
                language_name=d.pop("language_name", json_name),
            )

            match d.pop_optional("library_name", json_name):
                case names.Name() as lib_name:
                    result.library_name = lib_name

            match d.pop_optional("short_name", json_string):
                case str(short_name):
                    result.short_name = short_name

            match d.pop_optional("version", json_string):
                case str(version):
                    result.version = version

            match d.pop_optional("build_date", json_string):
                case str(build_date):
                    result.build_date = build_date

            match d.pop_optional("standalone", json_boolean):
                case bool(standalone):
                    result.standalone = standalone

            match d.pop_optional("defaults", LibraryDefaults.from_json):
                case LibraryDefaults() as defaults:
                    result.defaults = defaults

            match d.pop_optional(
                "symbol_canonicalizer", LibraryEntity.from_json
            ):
                case LibraryEntity() as symbol_canonicalizer:
                    result.symbol_canonicalizer = symbol_canonicalizer

            match d.pop_optional(
                "external_exceptions", json_list(ExternalException.from_json)
            ):
                case list() as external_exceptions:
                    result.external_exceptions = external_exceptions

            match d.pop_optional(
                "property_exceptions",
                json_list(json_string),
            ):
                case list() as property_exceptions:
                    result.property_exceptions = property_exceptions

            match d.pop_optional(
                "extra_context_clauses",
                json_dict(UnitContextClauses.from_json),
            ):
                case dict() as extra_context_clauses:
                    result.extra_context_clauses = extra_context_clauses

            match d.pop_optional(
                "cache_collection", CacheCollectionConfig.from_json
            ):
                case CacheCollectionConfig() as cache_collection:
                    result.cache_collection = cache_collection

            match d.pop_optional(
                "extra_install_files", json_dict(json_list(json_string))
            ):
                case dict() as extra_install_files:
                    result.extra_install_files = extra_install_files

        return result


@dataclasses.dataclass
class LktSpecConfig:
    """
    Configuration for Lkt input sources.
    """

    entry_point: str
    """
    Lkt source file from which to start analysis.
    """

    source_dirs: list[str] = dataclasses.field(default_factory=list)
    """
    List of directories to add to the Lkt search path.
    """

    types_from_lkt: bool = False
    """
    When loading definitions from Lktlang files, whether to load type
    definitions. This is not done by default during the transition from our
    Python DSL to Lktlang.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> LktSpecConfig | None:
        if json is None:
            return None

        with JSONDictDecodingContext(context, json) as d:
            result = cls(entry_point=d.pop("entry_point", json_string))

            match d.pop_optional("source_dirs", json_list(json_string)):
                case list() as source_dirs:
                    result.source_dirs = source_dirs

            match d.pop_optional("types_from_lkt", json_boolean):
                case bool(types_from_lkt):
                    result.types_from_lkt = types_from_lkt

            return result


@dataclasses.dataclass
class MainsConfig:
    """
    Configuration for the "mains" project (used to build programs on top of the
    generated library).
    """

    source_dirs: list[str] = dataclasses.field(default_factory=list)
    """
    List of directories to add as source directories for the "mains" project.
    """

    main_programs: list[str] = dataclasses.field(default_factory=list)
    """
    Unit name for the Ada source files to build as programs in the "mains"
    project.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> MainsConfig:
        with JSONDictDecodingContext(context, json) as d:
            result = cls()

            match d.pop_optional("source_dirs", json_list(json_string)):
                case list() as source_dirs:
                    result.source_dirs = source_dirs

            match d.pop_optional("main_programs", json_list(json_string)):
                case list() as main_programs:
                    result.main_programs = main_programs

            return result


@dataclasses.dataclass
class EmissionConfig:
    """
    All configuration that allows to run code generation for the library to
    generate.
    """
    library_directory: str = "build"
    """
    Root directory in which the library should be generated, relative to the
    language root directory.
    """

    source_post_processors: dict[str, str] = dataclasses.field(
        default_factory=dict
    )
    """
    By-language optional post-processing callbacks for generated sources.

    Post-processors are designated as strings for plugin references.
    """

    generate_auto_dll_dirs: bool = False
    """
    Whether to generate a code snippet in Python bindings to automatically add
    directories of the 'PATH' environment variable to the DLL searching
    directories on Windows systems.
    """

    show_property_logging: bool = False
    """
    If true, any property that has been marked with tracing activated will be
    traced on stdout by default, without need for any config file.
    """

    coverage: bool = False
    """
    Instrument the generated library to compute its code coverage. This
    requires GNATcoverage.
    """

    relative_project: bool = False
    """
    Use relative paths in generated project files. This is useful in order to
    get portable generated sources, for releases for instance.
    """

    rst_passthrough_roles: list[str] = dataclasses.field(default_factory=list)
    """
    List of names for the RST "passthrough" roles to register in docutils.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> EmissionConfig:
        with JSONDictDecodingContext(context, json) as d:
            result = cls()

            match d.pop_optional("library_directory", json_string):
                case str(library_directory):
                    result.library_directory = library_directory

            match d.pop_optional(
                "source_post_processors", json_dict(json_string)
            ):
                case dict() as source_post_processors:
                    result.source_post_processors = source_post_processors

            match d.pop_optional("generate_auto_dll_dirs", json_boolean):
                case bool(generate_auto_dll_dirs):
                    result.generate_auto_dll_dirs = generate_auto_dll_dirs

            match d.pop_optional("show_property_logging", json_boolean):
                case bool(show_property_logging):
                    result.show_property_logging = show_property_logging

            match d.pop_optional("coverage", json_boolean):
                case bool(coverage):
                    result.coverage = coverage

            match d.pop_optional("relative_project", json_boolean):
                case bool(relative_project):
                    result.relative_project = relative_project

            match d.pop_optional(
                "rst_passthrough_roles", json_list(json_string)
            ):
                case list() as rst_passthrough_roles:
                    result.rst_passthrough_roles = rst_passthrough_roles

            return result


@dataclasses.dataclass
class ManageDefaults:
    """
    Default behavior for Langkit's manage scripts.
    """

    build_warnings: bool = False
    """
    Whether warnings to build the generated library are enabled by default.
    """

    enable_java: bool = False
    """
    Whether to build Java bindings by default.
    """

    @classmethod
    def from_json(cls, context: str, json: object) -> ManageDefaults:
        with JSONDictDecodingContext(context, json) as d:
            result = cls()

            match d.pop_optional("build_warnings", json_boolean):
                case bool(build_warnings):
                    result.build_warnings = build_warnings

            match d.pop_optional("enable_java", json_boolean):
                case bool(enable_java):
                    result.enable_java = enable_java

            return result


@dataclasses.dataclass
class CompilationConfig:
    """
    All configuration that allows to compile/analyze the library to generate.
    """

    lkt_spec: LktSpecConfig | None
    """
    Configuration for Lkt input sources, or None if there is no Lkt source to
    generate the library.
    """

    library: LibraryConfig
    """
    Configuration for the library to generate.
    """

    mains: MainsConfig = dataclasses.field(default_factory=MainsConfig)

    plugin_passes: list[str] = dataclasses.field(default_factory=list)
    """
    Additional compilation passes to run during code emission.

    Passes are designated as strings for plugin references.
    """

    optional_passes: dict[str, bool] = dataclasses.field(default_factory=dict)
    """
    For optional compilation passes to explicitly enable/disable, mapping from
    the pass name to whether to enable it.
    """

    warnings: dict[str, bool] = dataclasses.field(default_factory=dict)
    """
    For warnings to explicitly enable/disable, mapping from the warning name to
    the whether to enable it.
    """

    extra_docs: dict[str, str] = dataclasses.field(default_factory=dict)
    """
    If provided, supply templates to document entities in the generated code.
    These will be added to the documentations available in code generation: see
    langkit.documentation.
    """

    emission: EmissionConfig = dataclasses.field(
        default_factory=EmissionConfig
    )

    manage_defaults: ManageDefaults = dataclasses.field(
        default_factory=ManageDefaults
    )

    def resolve_paths(self, base_directory: str) -> None:
        """
        Resolve all paths into absolute paths. All paths in the config are
        interpreted as relative to the library root directory, which is itself
        interpreted as relative to "base_directory".

        Note that even though this operation mutates ``self``, subsequent calls
        (even for different base directories) have no effect.
        """
        base_dir = os.path.abspath(base_directory)
        root_dir = os.path.join(base_dir, self.library.root_directory)
        self.library.root_directory = root_dir
        self.library.extra_install_files = {
            dest_dir: [
                os.path.join(base_directory, f)
                for f in files
            ]
            for dest_dir, files in self.library.extra_install_files.items()
        }
        if self.lkt_spec:
            self.lkt_spec.entry_point = os.path.join(
                root_dir, self.lkt_spec.entry_point
            )
            self.lkt_spec.source_dirs = [
                os.path.join(root_dir, d) for d in self.lkt_spec.source_dirs
            ]
        self.emission.library_directory = os.path.join(
            root_dir, self.emission.library_directory
        )
        self.mains.source_dirs = [
            os.path.join(root_dir, d) for d in self.mains.source_dirs
        ]

    @classmethod
    def from_json(
        cls,
        context: str,
        json: object,
        base_directory: str = ".",
    ) -> CompilationConfig:
        with JSONDictDecodingContext(context, json) as d:
            result = cls(
                lkt_spec=d.pop("lkt_spec", LktSpecConfig.from_json),
                library=d.pop(
                    "library",
                    # The call to "resolve_paths" below will resolve the
                    # following base directory as relative to our
                    # "base_directory" argument, so ".". is always the right
                    # unresolved library directory here.
                    LibraryConfig.from_json(base_directory="."),
                ),
            )

            match d.pop_optional("mains", MainsConfig.from_json):
                case MainsConfig() as mains:
                    result.mains = mains

            match d.pop_optional("plugin_passes", json_list(json_string)):
                case list() as plugin_passes:
                    result.plugin_passes = plugin_passes

            match d.pop_optional("optional_passes", json_dict(json_boolean)):
                case dict() as optional_passes:
                    result.optional_passes = optional_passes

            match d.pop_optional("warnings", json_dict(json_boolean)):
                case dict() as warnings:
                    result.warnings = warnings

            match d.pop_optional("extra_docs", json_dict(json_string)):
                case dict() as extra_docs:
                    result.extra_docs = extra_docs

            match d.pop_optional("emission", EmissionConfig.from_json):
                case EmissionConfig() as emission:
                    result.emission = emission

            match d.pop_optional("manage_defaults", ManageDefaults.from_json):
                case ManageDefaults() as manage_defaults:
                    result.manage_defaults = manage_defaults

            result.resolve_paths(base_directory)
            return result


def add_args(parser: argparse.ArgumentParser) -> None:
    """
    Add arguments for compilation configuration to "parser".
    """
    from langkit.passes import PassManager

    parser.add_argument(
        "--build-dir",
        default="build",
        help=(
            "Directory to use for generated source code and binaries, relative"
            " to the language spec's root directory. By default, use 'build'."
        ),
    )

    parser.add_argument(
        "--plugin-pass",
        action="append",
        help=(
            "Fully qualified name to a Langkit plug-in pass constructor. The"
            " function must return a Langkit pass, whose type derives from"
            " langkit.passes.AbstractPass. It will be ran at the end of the"
            " pass preexisting order."
        ),
    )
    PassManager.add_args(parser)

    parser.add_argument(
        "--generate-auto-dll-dirs",
        action="store_true",
        help=(
            "For Python bindings on Windows. Add a code snippet which uses the"
            " 'os.add_dll_directory' function to append directories of the"
            " 'PATH' environment variable to DLL searching places."
        ),
    )

    WarningSet.add_args(parser)

    parser.add_argument(
        "--coverage",
        action="store_true",
        help=(
            "Instrument the generated library to compute its code coverage."
            " This requires GNATcoverage."
        ),
    )

    parser.add_argument(
        "--relative-project",
        action="store_true",
        help=(
            "Use relative paths in generated project files. This is useful in"
            " order to get portable generated sources, for releases for"
            " instance."
        ),
    )

    parser.add_argument(
        "--version", help="Version number for the generated library."
    )
    parser.add_argument(
        "--build-date", help="Build date number for the generated library."
    )


def update_config_from_args(
    config: CompilationConfig,
    args: argparse.Namespace,
) -> None:
    """
    Update ``config`` using compilation arguments found in ``args``.
    """
    config.emission.library_directory = os.path.join(
        config.library.root_directory, args.build_dir
    )
    config.plugin_passes += args.plugin_pass or []
    config.optional_passes.update(args.pass_activations)

    if args.generate_auto_dll_dirs:
        config.emission.generate_auto_dll_dirs = True

    config.warnings.update(args.warning_activations)

    if args.coverage:
        config.emission.coverage = True
    if args.relative_project:
        config.emission.relative_project = True

    with diagnostic_context(Location.nowhere):
        if args.version:
            if (
                config.library.version != "undefined"
                and config.library.version != args.version
            ):
                error(
                    f"Got conflicting versions:"
                    f" {args.version!r} and {config.library.version!r}"
                )
            config.library.version = args.version
        if args.build_date:
            if (
                config.library.build_date != "undefined"
                and config.library.build_date != args.build_date
            ):
                error(
                    f"Got conflicting build_date:"
                    f" {args.build_date!r} and {config.library.build_date!r}"
                )
            config.library.build_date = args.build_date
