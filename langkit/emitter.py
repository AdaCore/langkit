"""
Code emission for Langkit-generated libraries.
"""

from __future__ import annotations

import glob
import json
import os
from os import path
from typing import Any

from langkit.caching import Cache
from langkit.compile_context import AdaSourceKind, CompileCtx, Verbosity
from langkit.coverage import InstrumentationMetadata
from langkit.diagnostics import Location, error
from langkit.generic_api import GenericAPI
from langkit.lexer.regexp import DFACodeGenHolder
import langkit.names as names
from langkit.template_utils import add_template_dir
from langkit.utils import Colors, Language, printcol


@CompileCtx.register_template_extensions
def template_extensions(ctx: CompileCtx) -> dict[str, Any]:
    return {"generic_api": GenericAPI(ctx)}


class Emitter:
    """
    Code and data holder for code emission.
    """

    def __init__(self, context: CompileCtx):
        """
        Generate sources for the analysis library. Also emit a tiny program
        useful for testing purposes.

        :param context: Compilation context that owns this emitter.
        """
        config = context.config

        self.context = context
        self.verbosity = context.verbosity

        self.standalone_support_name = (
            f"{self.context.ada_api_settings.lib_name}_Support"
        )
        """
        Name of the replacement for Langkit_Support in standalone mode.
        """

        self.standalone_adasat_name = (
            f"{self.context.ada_api_settings.lib_name}_AdaSAT"
        )
        """
        Name of the replacement for AdaSAT in standalone mode.
        """

        self.lib_root = config.emission.library_directory
        self.cache = Cache(os.path.join(self.lib_root, "obj", "langkit_cache"))
        self.source_post_processors = context.source_post_processors
        self.extensions_dir = context.extensions_dir

        # TODO: contain the add_template_dir calls to this context (i.e. avoid
        # global mutation).

        if self.extensions_dir:
            add_template_dir(self.extensions_dir)

        self.generate_unparsers = context.generate_unparsers
        self.generate_auto_dll_dirs = config.emission.generate_auto_dll_dirs
        self.coverage = config.emission.coverage
        self.gnatcov = context.gnatcov
        self.portable_project = config.emission.portable_project

        # Automatically add all source files in the "extensions/src" directory
        # to the generated library project.
        self.extensions_src_dir = None
        if self.extensions_dir:
            src_dir = path.join(self.extensions_dir, "src")
            if path.isdir(src_dir):
                self.extensions_src_dir = src_dir
                for filename in os.listdir(src_dir):
                    filepath = path.join(src_dir, filename)
                    if path.isfile(filepath) and not filename.startswith("."):
                        self.context.additional_source_files.append(filepath)

        self.main_programs = set(config.mains.main_programs)
        self.main_programs.add("parse")
        if self.generate_unparsers:
            self.main_programs.add("unparse")

        self.lib_name_low = context.ada_api_settings.lib_name.lower()
        """
        Lower-case name for the generated library.
        """

        self.lib_name_up = context.ada_api_settings.lib_name.upper()
        """
        Upper-case name for the generated library.
        """

        # Paths for the various directories in which code is generated
        self.src_dir = path.join(self.lib_root, "src")
        self.src_mains_dir = path.join(self.lib_root, "src-mains")
        self.scripts_dir = path.join(self.lib_root, "scripts")
        self.python_dir = path.join(self.lib_root, "python")
        self.python_pkg_dir = path.join(
            self.python_dir, context.python_api_settings.module_name
        )
        self.ocaml_dir = path.join(self.lib_root, "ocaml")

        self.java_dir = path.join(self.lib_root, "java")
        self.java_package = path.join(
            self.java_dir,
            "src",
            "main",
            "java",
            "com",
            "adacore",
            self.lib_name_low,
        )
        self.java_jni = path.join(self.java_dir, "jni")

        self.lib_project = path.join(self.lib_root, f"{self.lib_name_low}.gpr")
        self.mains_project = path.join(self.lib_root, "mains.gpr")

        # Get source directories for the mains project file. making them
        # relative to the generated project file (which is
        # $BUILD_DIR/mains.gpr).
        self.main_source_dirs = {
            os.path.relpath(
                os.path.join(config.library.root_directory, sdir),
                os.path.dirname(self.mains_project),
            )
            for sdir in config.mains.source_dirs
        }

        self.dfa_code: DFACodeGenHolder
        """
        Holder for the data structures used to generate code for the lexer
        state machine (DFA). As an optimization, it is left to None if we
        decide not to generate it (i.e. when the already generated sources are
        up-to-date).
        """

        self._project_file_emitted = False
        """
        Whether we emitted a project file for the generated library.
        """

        self.project_languages = {"Ada", "C"}
        """
        List of GPR names for languages used in the generated library.

        The core library is made of Ada and C sources, so always include them.
        """

        self.library_interfaces = set()
        """
        Set of source file base names for all sources that must appear in the
        "Interfaces" attribute of the generated library project file.
        """

        self.instr_md = InstrumentationMetadata()

        # Add all additional source files to the list of library interfaces and
        # declare them as such in instrumentation metadata.
        for f in context.additional_source_files:
            self.add_library_interface(f, generated=False)

        if self.coverage:
            assert self.gnatcov
            # Add the buffer-list unit from GNATcoverage's instrumentation to
            # the list of library interfaces. TODO: hopefully, we should not
            # have to do this anymore after S916-064 is addressed.
            self.library_interfaces.add(self.gnatcov.buffer_list_file(self))

        self.main_project_file = os.path.join(
            self.lib_root, f"{self.lib_name_low}.gpr"
        )

        extension_unit = (
            f"{context.ada_api_settings.lib_name}.Implementation.Extensions"
        )

        # Determine whether we have user external properties. If so,
        # automatically WITH $.Implementation.Extensions from the body of
        # $.Analysis and $.Implementation.
        if any(
            prop.user_external
            for prop in context.all_properties(include_inherited=True)
        ):
            for unit in ("Analysis", "Implementation", "Implementation.C"):
                context.add_with_clause(
                    unit, AdaSourceKind.body, extension_unit, use_clause=True
                )

        # Likewise, if we have custom "_Short_Image" functions from extensions,
        # $.Implementation needs to have visibility over it.
        if any(
            node.annotations.custom_short_image for node in context.node_types
        ):
            context.add_with_clause(
                "Implementation",
                AdaSourceKind.body,
                extension_unit,
                use_clause=True,
            )

        # Determine the default unparsing configuration
        unparsing_cfg_filename = (
            self.context.config.library.defaults.unparsing_config
        )
        if unparsing_cfg_filename is None:
            self.default_unparsing_config = b'{"node_configs": {}}'
        else:
            assert self.context.extensions_dir
            with open(
                path.join(self.context.extensions_dir, unparsing_cfg_filename),
                "rb",
            ) as fp:
                self.default_unparsing_config = fp.read()

    def path_to(self, destination: str, path_from: str) -> str:
        """
        Helper to generate absolute or relative paths inside the generated
        project, depending on libmanage's --relative-path.

        :param destination: Path to generate. This argument can be either
            relative to ``path_from`` or absolute.
        """
        destination = os.path.abspath(os.path.join(path_from, destination))
        return (
            os.path.relpath(destination, path_from)
            if self.portable_project
            else destination
        )

    def add_library_interface(
        self, filename: str, generated: bool, is_ada: bool = True
    ) -> None:
        assert not self._project_file_emitted

        filename = os.path.basename(filename)

        # Register Ada source files in the appropriate instrumentation metadata
        # set for coverage reports.
        if is_ada:
            source_set = (
                self.instr_md.generated_sources
                if generated
                else self.instr_md.additional_sources
            )
            source_set.add(filename)

            # Add GNATcoverage's additional buffer units to the library
            # interface.  TODO: hopefully, we should not have to do this
            # anymore after S916-064 is addressed.
            if self.coverage:
                assert self.gnatcov
                for f in self.gnatcov.buffer_files(filename):
                    self.library_interfaces.add(f)

        self.library_interfaces.add(filename)

    def setup_directories(self, ctx: CompileCtx) -> None:
        """
        Make sure the tree of directories needed for code generation exists.
        """
        if not path.exists(self.lib_root):
            os.mkdir(self.lib_root)

        for d in [
            self.src_dir,
            self.src_mains_dir,
            self.scripts_dir,
            os.path.join(self.lib_root, "obj"),
            self.python_dir,
            self.python_pkg_dir,
            self.java_dir,
            self.java_package,
            self.java_jni,
        ]:
            if not path.exists(d):
                os.makedirs(d)

    def merge_library_sources(
        self, library_dir: str, merged_name: str
    ) -> None:
        """
        Merge all source files from ``library_dir`` to the generated library's
        source directory, and avoid conflicts with original unit names by
        renaming the base name to ``merged_name``.
        """
        for source_kind, ext in [
            (AdaSourceKind.spec, "ads"),
            (AdaSourceKind.body, "adb"),
        ]:
            for file in glob.glob(os.path.join(library_dir, f"*.{ext}")):
                # Build a qualified (Ada) name for the imported unit
                unit_name, _ = os.path.splitext(os.path.basename(file))
                qual_name = unit_name.split("-")
                qual_name[0] = merged_name

                # Register the imported unit as an interface in the generated
                # project.
                self.add_library_interface(
                    self.ada_file_path(self.src_dir, source_kind, qual_name),
                    generated=True,
                )

                with open(file) as f:
                    content = f.read()
                self.write_ada_file(
                    self.src_dir,
                    source_kind,
                    qual_name,
                    content,
                    # We are creating copies of existing sources. These sources
                    # already have attributes that post processors are supposed
                    # to add (e.g. license headers), so we should not run post
                    # processors for them.
                    no_post_processing=True,
                )

    @property
    def support_dir(self) -> str:
        """
        Return the directory that contains the langkit_support library.
        """
        return os.path.join(
            os.path.dirname(os.path.realpath(__file__)), "support"
        )

    def merge_support_libraries(self, ctx: CompileCtx) -> None:
        """
        In standalone mode only, copy all units from Langkit_Support and AdaSAT
        into the generated library. Imported units are renamed to avoid clashes
        with Langkit_Support and AdaSAT themselves.
        """
        if not self.context.config.library.standalone:
            return

        default_adasat_dir = os.path.join(
            os.path.dirname(os.path.realpath(__file__)), "adasat", "src"
        )

        # We expect AdaSAT checkout to be at <langkit_root>/langkit/adasat
        # If it's not found there, we look for "adasat.gpr" in the
        # GPR_PROJECT_PATH environment variable.
        adasat_dir: str | None
        if os.path.exists(default_adasat_dir):
            adasat_dir = default_adasat_dir
        else:
            gpr_paths_var = os.environ.get("GPR_PROJECT_PATH")
            gpr_paths = (
                gpr_paths_var.split(os.path.pathsep) if gpr_paths_var else []
            )
            adasat_dir = next(
                (
                    os.path.join(path, "src")
                    for path in gpr_paths
                    if os.path.exists(os.path.join(path, "adasat.gpr"))
                ),
                None,
            )

        if adasat_dir is None:
            error(
                "AdaSAT must be checked out at"
                " '<langkit_root>/langkit/adasat' or its project file be"
                " reachable from the 'GPR_PROJECT_PATH' environment variable"
                " in order to build a standalone library.",
                location=Location.nowhere,
                ok_for_codegen=True,
            )

        self.merge_library_sources(
            self.support_dir, self.standalone_support_name
        )
        self.merge_library_sources(adasat_dir, self.standalone_adasat_name)

    def emit_lib_project_file(self, ctx: CompileCtx) -> None:
        """
        Emit a project file for the generated library.
        """
        self._project_file_emitted = True
        self.write_source_file(
            self.main_project_file,
            ctx.render_template(
                "project_file",
                lib_name=ctx.ada_api_settings.lib_name,
                os_path=os.path,
                project_path=os.path.dirname(self.main_project_file),
            ),
            language=None,
        )

    def instrument_for_coverage(self, ctx: CompileCtx) -> None:
        """
        If code coverage is enabled, instrument the generated library with
        GNATcoverage.
        """
        if not self.coverage:
            return
        assert self.gnatcov
        self.gnatcov.instrument(
            self, os.path.join(self.lib_root, "obj", "instr")
        )

    def generate_lexer_dfa(self, ctx: CompileCtx) -> None:
        """
        Generate code for the lexer state machine.
        """
        assert ctx.lexer

        # Source file that contains the state machine implementation
        lexer_sm_body = self.ada_file_path(
            self.src_dir,
            AdaSourceKind.body,
            [ctx.lib_name.camel_with_underscores, "Lexer_State_Machine"],
        )

        # Generate the lexer state machine iff the file is missing or its
        # signature has changed since last time.
        stale_lexer_spec = self.write_source_file(
            os.path.join(
                self.lib_root,
                "obj",
                "{}_lexer_signature.txt".format(ctx.short_name_or_long),
            ),
            json.dumps(ctx.lexer.signature, indent=2),
            language=None,
        )
        if not os.path.exists(lexer_sm_body) or stale_lexer_spec:
            self.dfa_code = ctx.lexer.build_dfa_code(ctx)

    def emit_ada_lib(self, ctx: CompileCtx) -> None:
        """
        Emit Ada sources for the generated library.
        """

        class Unit:
            def __init__(
                self,
                template_base_name: str,
                rel_qual_name: str,
                has_body: bool = True,
                unparser: bool = False,
                cached_body: bool = False,
                is_interface: bool = True,
            ):
                """
                :param template_base_name: Common prefix for the name of the
                    templates to use in order to generate spec/body sources for
                    this unit.

                :param rel_qual_name: Qualified name for the unit to generate,
                    without the top-level library name.

                :param unparser: Whether we can avoid generating this unit if
                    unparsing is disabled.

                :param has_body: Whether this unit has a body (otherwise, it's
                    just a spec).

                :param cached_body: If true, only register the body as a
                    library interface, i.e. do not generate it, considering
                    that it is cached.

                :param is_interface: Whether to include this module in the
                    generated library interface.
                """
                self.template_base_name = template_base_name
                self.qual_name = (
                    rel_qual_name.split(".") if rel_qual_name else []
                )
                self.unparser = unparser
                self.has_body = has_body
                self.cached_body = cached_body
                self.is_interface = is_interface

        for u in [
            # Top (pure) package
            Unit("pkg_main", "", has_body=False),
            # Unit for declarations used by Analysis and Implementation
            Unit("pkg_common", "Common"),
            # Unit for public analysis primitives
            Unit("pkg_analysis", "Analysis"),
            # Unit for converters between public Ada types and C API-level ones
            Unit("pkg_c", "C"),
            # Unit for implementation of analysis primitives
            Unit("pkg_implementation", "Implementation"),
            # Unit for AST node iteration primitives
            Unit("pkg_iterators", "Iterators"),
            # Unit for converters between public and implementation types
            Unit("pkg_public_converters", "Public_Converters", has_body=True),
            Unit(
                "pkg_private_converters", "Private_Converters", has_body=True
            ),
            # Unit for AST rewriting primitives
            Unit("pkg_rewriting", "Rewriting", unparser=True),
            # Unit for unparsing tables
            Unit(
                "pkg_unparsers",
                "Unparsers",
                has_body=False,
                unparser=True,
                is_interface=False,
            ),
            # Unit for all parsers
            Unit("parsers/pkg_main", "Parsers"),
            Unit("parsers/pkg_impl", "Parsers_Impl", is_interface=False),
            # Units for the lexer
            Unit("pkg_lexer", "Lexer"),
            Unit("pkg_lexer_impl", "Lexer_Implementation"),
            Unit(
                "pkg_lexer_state_machine",
                "Lexer_State_Machine",
                has_body=True,
                cached_body=not hasattr(self, "dfa_code"),
            ),
            # Unit for debug helpers
            Unit("pkg_debug", "Debug"),
            # Unit for the Ada generic Langkit API
            Unit("pkg_generic_api", "Generic_API"),
            Unit(
                "pkg_generic_api_introspection",
                "Generic_API.Introspection",
                has_body=False,
            ),
            Unit("pkg_generic_impl", "Generic_Impl", is_interface=False),
            Unit(
                "pkg_generic_introspection",
                "Generic_Introspection",
                is_interface=False,
            ),
        ]:
            if not self.generate_unparsers and u.unparser:
                continue
            self.write_ada_module(
                self.src_dir,
                u.template_base_name,
                u.qual_name,
                u.has_body,
                u.cached_body,
                in_library=True,
                is_interface=u.is_interface,
            )

    def emit_mains(self, ctx: CompileCtx) -> None:
        """
        Emit sources and the project file for mains.
        """
        with names.camel_with_underscores:
            mains = [("Parse", "main_parse_ada")]
            if ctx.generate_unparsers:
                mains.append(("Unparse", "main_unparse_ada"))
            for unit_name, template_name in mains:
                self.write_ada_file(
                    path.join(self.lib_root, "src-mains"),
                    AdaSourceKind.body,
                    [unit_name],
                    ctx.render_template(template_name),
                )

        self.write_source_file(
            self.mains_project,
            ctx.render_template(
                "mains_project_file",
                lib_name=ctx.ada_api_settings.lib_name,
                source_dirs=self.main_source_dirs,
                main_programs=self.main_programs,
            ),
            language=None,
        )

        with open(path.join(self.support_dir, "gnat.adc")) as f:
            self.write_source_file(
                path.join(self.lib_root, "gnat.adc"),
                f.read(),
                language=None,
            )

    def emit_c_api(self, ctx: CompileCtx) -> None:
        """
        Generate header and binding body for the external C API.
        """

        def render(template_name: str) -> str:
            return ctx.render_template(template_name)

        with names.lower:
            header_filename = "{}.h".format(ctx.c_api_settings.lib_name)
            self.write_cpp_file(
                path.join(self.src_dir, header_filename),
                render("c_api/header_c"),
            )
            self.add_library_interface(
                header_filename, generated=True, is_ada=False
            )

        self.write_ada_module(
            self.src_dir,
            "c_api/pkg_main",
            ["Implementation", "C"],
            in_library=True,
        )

        if self.generate_unparsers:
            self.write_ada_module(
                self.src_dir,
                "c_api/pkg_rewriting",
                ["Rewriting_C"],
                in_library=True,
            )

    def emit_python_api(self, ctx: CompileCtx) -> None:
        """
        Generate the Python binding module.
        """

        def render_python_template(
            file_path: str, *args: Any, **kwargs: Any
        ) -> None:
            with names.camel:
                code = ctx.render_template(*args, **kwargs)
            self.write_python_file(file_path, code)

        # Emit the Python modules themselves
        render_python_template(
            os.path.join(self.python_pkg_dir, "__init__.py"),
            "python_api/module_py",
            c_api=ctx.c_api_settings,
            pyapi=ctx.python_api_settings,
            generate_auto_dll_dirs=self.generate_auto_dll_dirs,
            module_name=ctx.python_api_settings.module_name,
        )

        # Emit the empty "py.type" file so that users can easily leverage type
        # annotations in the generated bindings.
        self.write_source_file(
            os.path.join(self.python_pkg_dir, "py.typed"),
            "",
            language=None,
        )

        # Emit the setup.py script to easily install the Python binding
        setup_py_file = os.path.join(self.lib_root, "python", "setup.py")
        self.write_python_file(
            setup_py_file, ctx.render_template("python_api/setup_py")
        )

    def emit_python_playground(self, ctx: CompileCtx) -> None:
        """
        Emit sources for the Python playground script.
        """
        playground_file = os.path.join(
            self.scripts_dir, "{}_playground".format(ctx.short_name_or_long)
        )
        self.write_python_file(
            playground_file,
            ctx.render_template(
                "python_api/playground_py",
                module_name=ctx.python_api_settings.module_name,
            ),
        )
        os.chmod(playground_file, 0o775)

    def emit_gdb_helpers(self, ctx: CompileCtx) -> None:
        """
        Emit support files for GDB helpers.
        """
        lib_name = ctx.ada_api_settings.lib_name.lower()
        gdbinit_path = os.path.join(self.lib_root, "gdbinit.py")
        gdb_c_path = os.path.join(self.src_dir, "{}-gdb.c".format(lib_name))

        # Always emit the ".gdbinit.py" GDB script
        self.write_python_file(
            gdbinit_path,
            ctx.render_template(
                "gdb_py",
                langkit_path=os.path.dirname(os.path.dirname(__file__)),
                lib_name=lib_name,
                prefix=ctx.short_name_or_long,
            ),
        )

        # Unless the generated project is requested to be relocatable, generate
        # the C file to embed the absolute path to this script in the generated
        # library.
        if not self.portable_project:
            self.write_source_file(
                gdb_c_path,
                ctx.render_template(
                    "gdb_c", gdbinit_path=gdbinit_path, os_name=os.name
                ),
                Language.c_cpp,
            )

    def emit_ocaml_api(self, ctx: CompileCtx) -> None:
        """
        Generate binding for the external OCaml API.
        """
        ctx.ocaml_api_settings.init_type_graph()

        if not os.path.isdir(self.ocaml_dir):
            os.mkdir(self.ocaml_dir)

        with names.camel:
            # Write an empty ocamlformat file so we can call ocamlformat
            self.write_source_file(
                os.path.join(self.ocaml_dir, ".ocamlformat"),
                "",
                language=None,
            )

            code = ctx.render_template(
                "ocaml_api/module_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings,
            )

            ocaml_filename = "{}.ml".format(ctx.c_api_settings.lib_name)
            self.write_ocaml_file(
                os.path.join(self.ocaml_dir, ocaml_filename),
                code,
            )

            code = ctx.render_template(
                "ocaml_api/module_sig_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings,
            )

            ocaml_filename = "{}.mli".format(ctx.c_api_settings.lib_name)
            self.write_ocaml_file(
                os.path.join(self.ocaml_dir, ocaml_filename), code
            )

            # Emit dune file to easily compile and install bindings
            code = ctx.render_template(
                "ocaml_api/dune_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings,
            )

            self.write_source_file(
                os.path.join(self.ocaml_dir, "dune"),
                code,
                language=None,
            )
            self.write_source_file(
                os.path.join(self.ocaml_dir, "dune-project"),
                "(lang dune 1.6)",
                language=None,
            )

            # Write an empty opam file to install the lib with dune
            self.write_source_file(
                os.path.join(
                    self.ocaml_dir,
                    "{}.opam".format(ctx.c_api_settings.lib_name),
                ),
                "",
                language=None,
            )

    def emit_java_api(self, ctx: CompileCtx) -> None:
        """
        Generate the bindings to the Java environment.
        """
        for template, export_file, export_dir, language in [
            (
                "java_api/main_class",
                f"{ctx.lib_name.camel}.java",
                self.java_package,
                Language.java,
            ),
            ("java_api/pom_xml", "pom.xml", self.java_dir, None),
            ("java_api/makefile", "Makefile", self.java_dir, None),
            (
                "java_api/jni_impl_c",
                "jni_impl.c",
                self.java_jni,
                Language.c_cpp,
            ),
            ("java_api/readme_md", "README.md", self.java_dir, None),
        ]:
            code = ctx.render_template(
                template,
                c_api=ctx.c_api_settings,
                java_api=ctx.java_api_settings,
            )
            self.write_source_file(
                os.path.join(export_dir, export_file),
                code,
                language,
            )

    def write_ada_module(
        self,
        out_dir: str,
        template_base_name: str,
        qual_name: list[str],
        has_body: bool = True,
        cached_body: bool = False,
        in_library: bool = False,
        is_interface: bool = True,
    ) -> None:
        """
        Write an Ada module (both spec and body) using a standardized scheme
        for finding the corresponding templates.

        :param out_dir: The out directory for the generated module.

        :param template_base_name: The base name for the template, basically
            everything that comes before the _body_ada/_spec_ada component,
            including the directory.

        :param qual_name: Qualified name for the Ada module, as a list of
            "simple" package names. The base library name is automatically
            prepended to that list, so every generated module will be a child
            module of the base library module.

        :param has_body: If true, generate a body for this unit.

        :param cached_body: If true, only register the body as a library
            interface, i.e. do not generate it, considering that it is cached.

        :param is_interface: Whether to include this module in the generated
            library interface.
        """

        def do_emit(kind: AdaSourceKind) -> None:
            """
            Emit the "kind" source for this module.
            """
            qual_name_str = ".".join(qual_name)
            with_clauses = self.context.with_clauses[(qual_name_str, kind)]
            full_qual_name = [
                self.context.lib_name.camel_with_underscores
            ] + qual_name

            # When requested, register library module as library interfaces
            if is_interface and in_library:
                self.add_library_interface(
                    self.ada_file_path(out_dir, kind, full_qual_name),
                    generated=True,
                )

            # If asked not to generate the body, skip the rest
            if kind == AdaSourceKind.body and cached_body:
                return

            with names.camel_with_underscores:
                self.write_ada_file(
                    out_dir=out_dir,
                    source_kind=kind,
                    qual_name=full_qual_name,
                    content=self.context.render_template(
                        "{}{}_ada".format(
                            template_base_name +
                            # If the base name ends with a /, we don't
                            # put a "_" separator.
                            ("" if template_base_name.endswith("/") else "_"),
                            kind.value,
                        ),
                        with_clauses=with_clauses,
                    ),
                )

        do_emit(AdaSourceKind.spec)
        if has_body:
            do_emit(AdaSourceKind.body)

    def write_source_file(
        self,
        file_path: str,
        source: str,
        language: Language | None,
    ) -> bool:
        """
        Helper to write a source file.

        Return whether the file has been updated.

        :param file_path: Path of the file to write.
        :param source: Content of the file to write.
        :param language: Programming language for the source file to write, if
            applicable. None otherwise. Used to select the appropriate source
            post processor.
        """
        # Run the post-processor for this language, if there is one
        if language is not None:
            post_processor = self.source_post_processors.get(language)
            if post_processor is not None:
                source = post_processor.process(source)

        if not os.path.exists(file_path) or self.cache.is_stale(
            file_path, source
        ):
            if self.context.verbosity >= Verbosity.debug:
                printcol(
                    "Rewriting stale source: {}".format(file_path),
                    Colors.OKBLUE,
                )
            # Emit all source files as UTF-8 with "\n" line endings, no matter
            # the current platform.
            with open(file_path, "w", encoding="utf-8", newline="") as f:
                f.write(source)
            return True
        return False

    def write_python_file(self, file_path: str, source: str) -> None:
        """
        Helper to write a Python source file.

        :param file_path: Path of the file to write.
        :param source: Content of the file to write.
        """
        self.write_source_file(file_path, source, Language.python)

    def write_cpp_file(self, file_path: str, source: str) -> None:
        """
        Helper to write a C/C++ source file.

        :param file_path: Path of the file to write.
        :param source: Content of the file to write.
        """
        self.write_source_file(file_path, source, Language.c_cpp)

    def write_ocaml_file(self, file_path: str, source: str) -> None:
        """
        Helper to write a OCaml source file.

        :param file_path: Path of the file to write.
        :param source: Content of the file to write.
        """
        self.write_source_file(file_path, source, Language.ocaml)

    def ada_file_path(
        self, out_dir: str, source_kind: AdaSourceKind, qual_name: list[str]
    ) -> str:
        """
        Return the name of the Ada file for the given unit name/kind.

        :param out_dir: The complete path to the directory in which we want to
            write the file.
        :param source_kind: Determine whether the source is a spec or a body.
        :param qual_name: The qualified name of the Ada spec/body, as a list of
            strings.
        """
        file_name = "{}.{}".format(
            "-".join(n.lower() for n in qual_name),
            "ads" if source_kind == AdaSourceKind.spec else "adb",
        )
        return os.path.join(out_dir, file_name)

    def write_ada_file(
        self,
        out_dir: str,
        source_kind: AdaSourceKind,
        qual_name: list[str],
        content: str,
        no_post_processing: bool = False,
    ) -> None:
        """
        Helper to write an Ada file.

        :param out_dir: See ada_file_path.
        :param source_kind: See ada_file_path.
        :param qual_name: See ada_file_path.
        :param content: The source content to write to the file.
        :param no_post_processing: Whether to disable the source post
            processing when writing this file.
        """
        file_path = self.ada_file_path(out_dir, source_kind, qual_name)

        # In standalone mode, rename Langkit_Support occurences in the source
        # code. Likewise for ``"langkit_support__int__"``, used to build
        # external names.
        if self.context.config.library.standalone:
            for pattern, replacement in [
                ("Langkit_Support", self.standalone_support_name),
                (
                    '"langkit_support__int__"',
                    f'"{self.standalone_support_name.lower()}__int__"',
                ),
                ("AdaSAT", self.standalone_adasat_name),
            ]:
                content = content.replace(pattern, replacement)

        # If there are too many lines, which triggers obscure debug info bugs,
        # strip empty lines.
        lines = content.splitlines()
        if len(lines) > 200000:
            content = "\n".join(l for l in lines if l.strip())

        self.write_source_file(
            file_path,
            content,
            language=None if no_post_processing else Language.ada,
        )
