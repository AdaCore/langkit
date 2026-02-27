"""
Logic to represent and load configuration for a language specification.
"""

from __future__ import annotations

import argparse
import dataclasses
import enum
import os.path
import yaml

from langkit.diagnostics import DiagnosticContext, Location, WarningSet, error
from langkit.names import Name
from langkit.utils.deserialization import (
    DataclassFieldMetadata,
    DeserializationError,
    Deserializer,
)


# Dataclass field metadata to indicate that the cache for incremental code
# generation should skip this field, i.e. that changing the value of the field
# will not trigger the generation of libraries in an incremental setup. By
# default, all field value changes do trigger it.
_skip_cache_metadata_key = "langkit.config.skip_cache_metadata"
_skip_cache_metadata = {_skip_cache_metadata_key: True}


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
        return "{}.{}".format(self.unit_fqn, self.entity_name)

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

    @staticmethod
    def _deserialize(
        deserializer: Deserializer,
        context: str,
        obj: object,
    ) -> LibraryEntity:
        match obj:
            case str(obj):
                try:
                    return LibraryEntity.from_fqn(obj)
                except ValueError as exc:
                    deserializer.error(context, str(exc))
            case _:
                deserializer.type_error(context, obj, "string")


class IndentationKind(enum.StrEnum):
    """
    The kind of character to use when indenting.

    String values must reflect the corresponding Prettier enumeration values
    (``Prettier_Ada.Documents.Indentation_Kind``).
    """

    spaces = "Spaces"
    tabs = "Tabs"


class EndOfLineKind(enum.StrEnum):
    """
    The kind of character sequence to use as end-of-line marker.

    String values must reflect the corresponding Prettier enumeration values
    (``Prettier_Ada.Documents.End_Of_Line_Kind``).
    """

    LF = "LF"
    CR = "CR"
    CRLF = "CRLF"


@dataclasses.dataclass(frozen=True)
class FormatOptions:
    """
    Options for formatting sources of the specified language. These fields map
    to similarly named ones in ``Prettier_Ada.Documents.Format_Options_Type``.
    """

    line_width: int = 80
    """
    The maximum length allowed for a line.
    """

    indentation_kind: IndentationKind = IndentationKind.spaces
    """
    Indicates how to indent a line.
    """

    indentation_width: int = 2
    """
    If ``indentation_kind`` is "Spaces", the number of which to insert when
    indenting.

    If ``indentation_kind`` is "Tabs", the number of characters that a
    tabulation counts for when computing the length of a line.
    """

    indentation_continuation: int = 2
    """
    Same as above but for indentation that are used as continuation of a
    previous line.
    """

    end_of_line_kind: EndOfLineKind = EndOfLineKind.LF
    """
    Indicates how to end a line.
    """

    def __post_init__(self) -> None:
        assert self.line_width > 0
        assert self.indentation_width >= 0
        assert self.indentation_continuation >= 0


@dataclasses.dataclass(frozen=True)
class BuiltinUnparsingOverriding:
    """
    Unparsing configuration overriding that is built in the generated library.
    """

    filename: str
    """
    Filename (relative to the extension directory) that contains the JSON
    configuration overriding.
    """

    doc: str | None = None
    """
    Documentation for that builtin overriding (present in the generated Ada
    public API).
    """


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

    format_options: FormatOptions = FormatOptions()
    """
    When applying a tree rewriting diff, the options to use by default to
    format nodes that have no source correspondance or that are part of a
    source section that is deemed as needing to be reformatted.
    """


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


@dataclasses.dataclass
class ContextClause:
    """
    Description of a context clause to add to a generated unit.
    """

    with_unit: str = dataclasses.field(
        metadata=DataclassFieldMetadata.make(key="with")
    )
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


@dataclasses.dataclass
class LibraryConfig:
    """
    Configuration for the library to compile.
    """

    root_directory: str = dataclasses.field(
        metadata=DataclassFieldMetadata.make(auto_value=".")
    )
    """
    Root directory for the language spec.
    """

    language_name: Name
    """
    Name for the language to analyze.
    """

    library_name: Name | None = None
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
        default_factory=dict, metadata=_skip_cache_metadata
    )
    """
    Extra files to install when installing the library.

    This maps destination directories (relative to the installation prefix) to
    the files/directories to install (relative to the library root directory).
    """

    builtin_unparsing_overridings: dict[Name, BuiltinUnparsingOverriding] = (
        dataclasses.field(default_factory=dict)
    )
    """
    Unparsing configuration overridings that are built in the generated
    library.

    This mapping associates an entity name (camel with underscore convention)
    in ``$.Generic_API.Unparsing.Builtin_Overridings`` to the corresponding
    overriding information.
    """

    @property
    def actual_library_name(self) -> Name:
        return (
            Name("Lib{}lang".format(self.language_name.lower))
            if self.library_name is None
            else self.library_name
        )


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

    portable_project: bool = False
    """
    Use relative paths in generated source files. This is useful in order to
    get a truly portable/reproductible generated project, for releases for
    instance. The downside is that GDB hooks require more setup to work
    correctly.
    """

    rst_passthrough_roles: list[str] = dataclasses.field(default_factory=list)
    """
    List of names for the RST "passthrough" roles to register in docutils.
    """


@dataclasses.dataclass
class ManageDefaults:
    """
    Default behavior for Langkit's manage scripts.
    """

    build_warnings: bool = dataclasses.field(
        default=False, metadata=_skip_cache_metadata
    )
    """
    Whether warnings to build the generated library are enabled by default.
    """

    enable_java: bool = dataclasses.field(
        default=False, metadata=_skip_cache_metadata
    )
    """
    Whether to build Java bindings by default.

    Code for Java bindings is generated regardless of this configuration, so
    caching can disregard it.
    """


@dataclasses.dataclass
class LanguageServerConfig:
    """
    Configuration for the language server generated along the spec.
    """

    file_extensions: list[str] = dataclasses.field(default_factory=list)
    """
    List of file extensions that can be used to find files for the language
    server if there is no custom project manager.
    """


@dataclasses.dataclass
class CompilationConfig:
    """
    All configuration that allows to compile/analyze the library to generate.
    """

    lkt_spec: LktSpecConfig
    """
    Configuration for Lkt input sources.
    """

    library: LibraryConfig
    """
    Configuration for the library to generate.
    """

    language_server: LanguageServerConfig | None = None
    """
    Configuration for the language server.
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

    warnings: dict[str, bool] = dataclasses.field(
        default_factory=dict, metadata=_skip_cache_metadata
    )
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
            dest_dir: [os.path.join(base_directory, f) for f in files]
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
    def deserialize(
        cls,
        context: str,
        obj: object,
        base_directory: str = ".",
    ) -> CompilationConfig:
        try:
            result = _deserializer.deserialize(context, cls, obj)
        except DeserializationError as exc:
            error(str(exc), location=Location.nowhere)
        result.resolve_paths(base_directory)
        return result

    @classmethod
    def from_yaml_file(
        cls,
        filename: str,
    ) -> CompilationConfig:
        """
        Load a compilation configuration from a YAML file.

        :param filename: YAML file to load.
        """
        with open(filename) as f:
            obj = yaml.safe_load(f)
        return CompilationConfig.deserialize(
            context=os.path.basename(filename),
            obj=obj,
            base_directory=os.path.dirname(filename),
        )


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
        "--portable-project",
        action="store_true",
        help=(
            " Use relative paths in generated source files. This is useful in"
            " order to get a truly portable/reproductible generated project,"
            " for releases for instance. The downside is that GDB hooks"
            " require more setup to work correctly."
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
    if args.portable_project:
        config.emission.portable_project = True

    diag_ctx = DiagnosticContext(Location.nowhere)
    if args.version:
        if (
            config.library.version != "undefined"
            and config.library.version != args.version
        ):
            diag_ctx.error(
                f"Got conflicting versions: {args.version!r} and"
                f" {config.library.version!r}"
            )
        config.library.version = args.version
    if args.build_date:
        if (
            config.library.build_date != "undefined"
            and config.library.build_date != args.build_date
        ):
            diag_ctx.error(
                f"Got conflicting build_date: {args.build_date!r} and"
                f" {config.library.build_date!r}"
            )
        config.library.build_date = args.build_date


_deserializer = Deserializer()
_deserializer.add_type(Name, Name._deserialize)
_deserializer.add_type(LibraryEntity, LibraryEntity._deserialize)


def cache_summary(config: CompilationConfig) -> object:
    """
    Return a summary of the given configuration for caching purposes.

    The result has two properties:

    * The summaries of two configs must be equal if the only differences
      between these two configs are irrelevant to library compilation/ code
      generation.

    * A summary is JSON-serializable (so that it works with
      ``langkit.caching.Cache``).
    """

    def recurse(value: object) -> object:
        if dataclasses.is_dataclass(value):
            return {
                f.name: recurse(getattr(value, f.name))
                for f in dataclasses.fields(value)
                if not f.metadata.get(_skip_cache_metadata_key)
            }

        elif isinstance(value, Name):
            return value.lower

        elif isinstance(value, list):
            return [recurse(v) for v in value]

        elif isinstance(value, dict):
            result = {}
            for k, v in value.items():
                if not isinstance(k, str):
                    k = repr(k)
                result[k] = recurse(v)
            return result

        elif value is None or isinstance(value, (int, str)):
            return value

        else:
            assert False, f"unexpected config value for the cache: {value}"

    return recurse(config)
