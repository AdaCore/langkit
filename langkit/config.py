"""
Logic to represent and load configuration for a language specification.
"""

from __future__ import annotations

import argparse
import dataclasses
import os.path

from langkit.diagnostics import Location, WarningSet, diagnostic_context, error
import langkit.names as names


class LibraryEntity:
    """
    Reference to an entity in the generated library.
    """

    def __init__(self, unit_fqn: str, entity_name: str):
        """
        Create a reference to an entity in the generated library.

        :param unit_fqn: Fully qualified name for the unit that contains the
            referenced entity. For instance: "Libfoolang.My_Unit".
        :param entity_name: Simple name for the entity that is referenced.
        """
        self.unit_fqn = unit_fqn
        self.entity_name = entity_name

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
        unit, entity = value.rsplit(".", 1)
        return cls(unit, entity)


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

    version: str | None = None
    """
    String for the version of the generated library. This is "undefined" if
    left to None.
    """

    build_date: str | None = None
    """
    String for the generated library build date (where "build" includes source
    generation). This is "undefined" if left to None.
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

    property_exceptions: list[str] = dataclasses.field(default_factory=list)
    """
    In addition to ``Property_Error``, set of names for exceptions that
    properties are allowed to raise.
    """

    cache_collection: CacheCollectionConfig | None = None
    """
    If not None, setup the automatic cache collection mechanism with this
    configuration.
    """

    @property
    def actual_library_name(self) -> names.Name:
        return (
            names.Name('Lib{}lang'.format(self.language_name.lower))
            if self.library_name is None else
            self.language_name
        )


@dataclasses.dataclass
class LktConfig:
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

    relative_project: bool = False
    """
    Use relative paths in generated project files. This is useful in order to
    get portable generated sources, for releases for instance.
    """


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


@dataclasses.dataclass
class CompilationConfig:
    """
    All configuration that allows to compile/analyze the library to generate.
    """

    lkt: LktConfig | None
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
        if self.lkt:
            self.lkt.entry_point = os.path.join(root_dir, self.lkt.entry_point)
            self.lkt.source_dirs = [
                os.path.join(root_dir, d) for d in self.lkt.source_dirs
            ]
        self.emission.library_directory = os.path.join(
            root_dir, self.emission.library_directory
        )
        self.mains.source_dirs = [
            os.path.join(root_dir, d) for d in self.mains.source_dirs
        ]

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
                config.library.version is not None
                and config.library.version != args.version
            ):
                error(
                    f"Got conflicting versions:"
                    f" {args.version!r} and {config.library.version!r}"
                )
            config.library.version = args.version
        if args.build_date:
            if (
                config.library.build_date is not None
                and config.library.build_date != args.build_date
            ):
                error(
                    f"Got conflicting build_date:"
                    f" {args.build_date!r} and {config.library.build_date!r}"
                )
            config.library.build_date = args.build_date
