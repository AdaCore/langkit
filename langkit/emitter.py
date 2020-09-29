"""
Code emission for Langkit-generated libraries.
"""

from distutils.spawn import find_executable
import json
import os
from os import path
import subprocess
from typing import Any, Callable, Dict, List, Optional, Set

from langkit.caching import Cache
from langkit.compile_context import AdaSourceKind, CompileCtx, get_context
from langkit.coverage import InstrumentationMetadata
from langkit.diagnostics import Severity, check_source_language
from langkit.generic_api import GenericAPI
from langkit.lexer.regexp import DFACodeGenHolder
import langkit.names as names
from langkit.template_utils import add_template_dir
from langkit.utils import Colors, printcol


@CompileCtx.register_template_extensions
def template_extensions(ctx: CompileCtx) -> Dict[str, Any]:
    return {"generic_api": GenericAPI(ctx)}


PostProcessFn = Optional[Callable[[str], str]]


def write_source_file(file_path: str,
                      source: str,
                      post_process: PostProcessFn = None) -> bool:
    """
    Helper to write a source file.

    Return whether the file has been updated.

    :param file_path: Path of the file to write.
    :param source: Content of the file to write.
    :param post_process: If provided, callable used to transform the source
        file content just before writing it.
    """
    context = get_context()
    assert context.emitter
    if post_process:
        source = post_process(source)
    if (not os.path.exists(file_path) or
            context.emitter.cache.is_stale(file_path, source)):
        if context.verbosity.debug:
            printcol('Rewriting stale source: {}'.format(file_path),
                     Colors.OKBLUE)
        # Emit all source files as UTF-8 with "\n" line endings, no matter the
        # current platform.
        with open(file_path, 'w', encoding='utf-8', newline='') as f:
            f.write(source)
        return True
    return False


def write_cpp_file(file_path: str,
                   source: str,
                   post_process: PostProcessFn = None) -> None:
    """
    Helper to write a C/C++ source file.

    :param file_path: Path of the file to write.
    :param source: Content of the file to write.
    """
    if write_source_file(file_path, source, post_process):
        if find_executable('clang-format'):
            subprocess.check_call(['clang-format', '-i', file_path])


def write_ocaml_file(file_path: str,
                     source: str,
                     post_process: PostProcessFn = None) -> None:
    """
    Helper to write a OCaml source file.

    :param file_path: Path of the file to write.
    :param source: Content of the file to write.
    """
    if write_source_file(file_path, source, post_process):
        if find_executable('ocamlformat'):
            subprocess.check_call(['ocamlformat', '-i', file_path])


def ada_file_path(out_dir: str,
                  source_kind: AdaSourceKind,
                  qual_name: List[str]) -> str:
    """
    Return the name of the Ada file for the given unit name/kind.

    :param out_dir: The complete path to the directory in which we want to
        write the file.
    :param source_kind: Determine whether the source is a spec or a body.
    :param qual_name: The qualified name of the Ada spec/body, as a list of
        strings.
    """
    file_name = '{}.{}'.format(
        '-'.join(n.lower() for n in qual_name),
        'ads' if source_kind == AdaSourceKind.spec else 'adb'
    )
    return os.path.join(out_dir, file_name)


def write_ada_file(out_dir: str,
                   source_kind: AdaSourceKind,
                   qual_name: List[str],
                   content: str,
                   post_process: PostProcessFn = None) -> None:
    """
    Helper to write an Ada file.

    :param out_dir: See ada_file_path.
    :param source_kind: See ada_file_path.
    :param qual_name: See ada_file_path.
    :param content: The source content to write to the file.
    """
    file_path = ada_file_path(out_dir, source_kind, qual_name)

    # If there are too many lines, which triggers obscure debug info bugs,
    # strip empty lines.
    lines = content.splitlines()
    if len(lines) > 200000:
        content = '\n'.join(l for l in lines if l.strip())

    # TODO: no tool is able to pretty-print a single Ada source file
    write_source_file(file_path, content, post_process)


class Emitter:
    """
    Code and data holder for code emission.
    """

    def __init__(self,
                 context: CompileCtx,
                 lib_root: str,
                 extensions_dir: Optional[str],
                 main_source_dirs: Set[str] = set(),
                 main_programs: Set[str] = set(),
                 no_property_checks: bool = False,
                 generate_ada_api: bool = True,
                 generate_gdb_hook: bool = True,
                 pretty_print: bool = False,
                 post_process_ada: PostProcessFn = None,
                 post_process_cpp: PostProcessFn = None,
                 post_process_python: PostProcessFn = None,
                 post_process_ocaml: PostProcessFn = None,
                 coverage: bool = False,
                 relative_project: bool = False,
                 unparse_script: Optional[str] = None):
        """
        Generate sources for the analysis library. Also emit a tiny program
        useful for testing purposes.

        :param lib_root: Path of the directory in which the library should be
            generated.

        :param extensions_dir: Directory to contain extensions for code
            generation. If None is provided, assume there is no extension.

        :param main_source_dirs: List of source directories to use in the
            project file for mains. Source directories must be relative to the
            mains project file directory (i.e. $BUILD/src-mains).

        :param main_programs: List of names for programs to build in addition
            to the generated library. To each X program, there must be a X.adb
            source file in the $BUILD/src directory.

        :param no_property_checks: If True, do not emit safety checks in the
            generated code for properties. Namely, this disables null checks on
            field access.

        :param generate_ada_api: If True, generate the public Ada API. If False
            and there is no main to generate, do not generate this Ada API.

        :param generate_gdb_hook: Whether to generate the ".debug_gdb_scripts"
            section. Good for debugging, but better to disable for releases.

        :param pretty_print: If true, pretty-print the generated sources.

        :param post_process_ada: Optional post-processing for generated Ada
            source code.

        :param post_process_cpp: Optional post-processing for generated C++
            source code.

        :param post_process_python: Optional post-processing for generated
            Python source code.

        :param post_process_ocaml: Optional post-processing for generated
            OCaml source code.

        :param coverage: Instrument the generated library to compute its code
            coverage. This requires GNATcoverage.

        :param relative_project: See libmanage's --relative-project option.
        """
        self.context = context
        self.verbosity = context.verbosity

        self.lib_root = lib_root
        self.cache = Cache(
            os.path.join(self.lib_root, 'obj', 'langkit_cache')
        )

        self.extensions_dir = extensions_dir

        # TODO: contain the add_template_dir calls to this context (i.e. avoid
        # global mutation).

        if self.extensions_dir:
            add_template_dir(self.extensions_dir)

        for dirpath in self.context.template_lookup_extra_dirs:
          if dirpath:
            add_template_dir(dirpath)

        self.no_property_checks = no_property_checks
        self.generate_ada_api = generate_ada_api or bool(main_programs)
        self.generate_gdb_hook = generate_gdb_hook
        self.generate_unparser = context.generate_unparser
        self.pretty_print = pretty_print
        self.post_process_ada = post_process_ada
        self.post_process_cpp = post_process_cpp
        self.post_process_python = post_process_python
        self.post_process_ocaml = post_process_ocaml
        self.coverage = coverage
        self.gnatcov = context.gnatcov
        self.relative_project = relative_project

        # Automatically add all source files in the "extensions/src" directory
        # to the generated library project.
        self.extensions_src_dir = None
        if self.extensions_dir:
            src_dir = path.join(self.extensions_dir, 'src')
            if path.isdir(src_dir):
                self.extensions_src_dir = src_dir
                for filename in os.listdir(src_dir):
                    filepath = path.join(src_dir, filename)
                    if path.isfile(filepath) and not filename.startswith('.'):
                        self.context.additional_source_files.append(filepath)

        self.main_source_dirs = main_source_dirs
        self.main_programs = main_programs

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
            self.lib_root, "python", context.python_api_settings.module_name
        )
        self.ocaml_dir = path.join(self.lib_root, "ocaml")

        self.lib_project = path.join(self.lib_root, f"{self.lib_name_low}.gpr")
        self.mains_project = path.join(self.lib_root, "mains.gpr")

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

        :type: bool
        """

        self.project_languages = {'Ada'}
        """
        List of GPR names for languages used in the generated library.

        :type: set[str]
        """

        self.library_interfaces = set()
        """
        Set of source file base names for all sources that must appear in the
        "Interfaces" attribute of the generated library project file.

        :type: set[str]
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
            self.lib_root, f'{self.lib_name_low}.gpr'
        )

        self.unparse_script = unparse_script
        """
        RA22-015: If set to something else than None, then the "dsl unparse"
        pass will be run on the given script.

        :type: langkit.compile_context.UnparseScript|None
        """

        # Determine whether we have user external properties. If so,
        # automatically WITH $.Implementation.Extensions from the body of
        # $.Analysis and $.Implementation.
        if any(
            prop.user_external
            for prop in context.all_properties(include_inherited=True)
        ):
            for unit in ('Analysis', 'Implementation', 'Implementation.C'):
                context.add_with_clause(
                    unit, AdaSourceKind.body,
                    '{}.Implementation.Extensions'
                    .format(context.ada_api_settings.lib_name),
                    use_clause=True
                )

    def path_to(self, destination: str, path_from: str) -> str:
        """
        Helper to generate absolute or relative paths inside the generated
        project, depending on libmanage's --relative-path.

        :param destination: Path to generate. This argument can be either
            relative to ``path_from`` or absolute.
        """
        destination = os.path.abspath(os.path.join(path_from, destination))
        return (os.path.relpath(destination, path_from)
                if self.relative_project else
                destination)

    def add_library_interface(self,
                              filename: str,
                              generated: bool,
                              is_ada: bool = True) -> None:
        assert not self._project_file_emitted

        filename = os.path.basename(filename)

        # Register Ada source files in the appropriate instrumentation metadata
        # set for coverage reports.
        if is_ada:
            source_set = (self.instr_md.generated_sources
                          if generated else
                          self.instr_md.additional_sources)
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
        ]:
            if not path.exists(d):
                os.mkdir(d)

    def emit_lib_project_file(self, ctx: CompileCtx) -> None:
        """
        Emit a project file for the generated library.
        """
        self._project_file_emitted = True
        write_source_file(
            self.main_project_file,
            ctx.render_template(
                'project_file',
                lib_name=ctx.ada_api_settings.lib_name,
                os_path=os.path,
                project_path=os.path.dirname(self.main_project_file),
            )
        )

    def instrument_for_coverage(self, ctx: CompileCtx) -> None:
        """
        If code coverage is enabled, instrument the generated library with
        GNATcoverage.
        """
        if not self.coverage:
            return
        assert self.gnatcov
        self.gnatcov.instrument(self,
                                os.path.join(self.lib_root, 'obj', 'instr'))

    def generate_lexer_dfa(self, ctx: CompileCtx) -> None:
        """
        Generate code for the lexer state machine.
        """
        assert ctx.lexer

        # Source file that contains the state machine implementation
        lexer_sm_body = ada_file_path(
            self.src_dir, AdaSourceKind.body,
            [ctx.lib_name.camel_with_underscores, 'Lexer_State_Machine']
        )

        # Generate the lexer state machine iff the file is missing or its
        # signature has changed since last time.
        stale_lexer_spec = write_source_file(
            os.path.join(
                self.lib_root, 'obj',
                '{}_lexer_signature.txt'
                .format(ctx.short_name_or_long)),
            json.dumps(ctx.lexer.signature, indent=2)
        )
        if not os.path.exists(lexer_sm_body) or stale_lexer_spec:
            self.dfa_code = ctx.lexer.build_dfa_code(ctx)

    def emit_ada_lib(self, ctx: CompileCtx) -> None:
        """
        Emit Ada sources for the generated library.
        """

        class Unit:
            def __init__(self,
                         template_base_name: str,
                         rel_qual_name: str,
                         has_body: bool = True,
                         ada_api: bool = False,
                         unparser: bool = False,
                         cached_body: bool = False,
                         is_interface: bool = True):
                """
                :param template_base_name: Common prefix for the name of the
                    templates to use in order to generate spec/body sources for
                    this unit.

                :param rel_qual_name: Qualified name for the unit to generate,
                    without the top-level library name.

                :param ada_api: Whether we can avoid generating this unit if
                    the Ada API is disabled.

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
                self.qual_name = (rel_qual_name.split('.')
                                  if rel_qual_name
                                  else [])
                self.ada_api = ada_api
                self.unparser = unparser
                self.has_body = has_body
                self.cached_body = cached_body
                self.is_interface = is_interface

        for u in [
            # Top (pure) package
            Unit('pkg_main', '', has_body=False),
            # Unit for declarations used by Analysis and Implementation
            Unit('pkg_common', 'Common'),
            # Unit for public analysis primitives
            Unit('pkg_analysis', 'Analysis', ada_api=True),
            # Unit for converters between public Ada types and C API-level ones
            Unit('pkg_c', 'C', ada_api=True),
            # Unit for implementation of analysis primitives
            Unit('pkg_implementation', 'Implementation'),
            # Unit for AST introspection public API
            Unit('pkg_introspection', 'Introspection', ada_api=True),
            # Unit for AST introspection internal API
            Unit('pkg_introspection_impl', 'Introspection_Implementation'),
            # Unit for AST node iteration primitives
            Unit('pkg_iterators', 'Iterators', ada_api=True),
            # Unit for converters between public and implementation types
            Unit('pkg_public_converters', 'Public_Converters', has_body=True,
                 ada_api=True),
            Unit('pkg_private_converters', 'Private_Converters',
                 has_body=False),
            # Unit for AST rewriting primitives
            Unit('pkg_rewriting', 'Rewriting', ada_api=True, unparser=True),
            # Unit for AST rewriting implementation
            Unit('pkg_rewriting_impl', 'Rewriting_Implementation',
                 unparser=True, is_interface=False),
            # Unit for AST unparsing primitives
            Unit('pkg_unparsing', 'Unparsing', ada_api=True, unparser=True),
            # Unit for AST implementation of unparsing primitives
            Unit('pkg_unparsing_impl', 'Unparsing_Implementation',
                 unparser=True, is_interface=False),
            # Unit for all parsers
            Unit('parsers/pkg_main', 'Parsers'),
            # Units for the lexer
            Unit('pkg_lexer', 'Lexer', ada_api=True),
            Unit('pkg_lexer_impl', 'Lexer_Implementation'),
            Unit('pkg_lexer_state_machine', 'Lexer_State_Machine',
                 has_body=True, cached_body=not hasattr(self, "dfa_code")),
            # Unit for debug helpers
            Unit('pkg_debug', 'Debug'),
            # Unit for the Ada generic Langkit API
            Unit('pkg_generic_api', 'Generic_API', ada_api=True),
            Unit('pkg_generic_impl', 'Generic_Impl',
                 ada_api=True, is_interface=False),
            Unit('pkg_generic_introspection', 'Generic_Introspection',
                 ada_api=True, is_interface=False),
        ]:
            if (
                (not self.generate_ada_api and u.ada_api) or
                (not self.generate_unparser and u.unparser)
            ):
                continue
            self.write_ada_module(self.src_dir, u.template_base_name,
                                  u.qual_name, u.has_body, u.cached_body,
                                  in_library=True, is_interface=u.is_interface)

    def emit_mains(self, ctx: CompileCtx) -> None:
        """
        Emit sources and the project file for mains.
        """
        with names.camel_with_underscores:
            write_ada_file(
                path.join(self.lib_root, 'src-mains'),
                AdaSourceKind.body, ['Parse'],
                ctx.render_template('main_parse_ada'),
                self.post_process_ada
            )

        write_source_file(
            self.mains_project,
            ctx.render_template(
                'mains_project_file',
                lib_name=ctx.ada_api_settings.lib_name,
                source_dirs=self.main_source_dirs,
                main_programs=self.main_programs
            )
        )

    def emit_c_api(self, ctx: CompileCtx) -> None:
        """
        Generate header and binding body for the external C API.
        """
        def render(template_name: str) -> str:
            return ctx.render_template(template_name)

        with names.lower:
            # TODO (TA20-017: gprinstall bug): generate the header in
            # "src" and add it to the library interface (see disabled code
            # below).
            header_filename = '{}.h'.format(ctx.c_api_settings.lib_name)
            write_cpp_file(
                path.join(self.lib_root, header_filename),
                render('c_api/header_c'),
                self.post_process_cpp
            )

        self.write_ada_module(
            self.src_dir, 'c_api/pkg_main',
            ['Implementation', 'C'],
            in_library=True
        )

    def emit_python_api(self, ctx: CompileCtx) -> None:
        """
        Generate the Python binding module.
        """
        def pretty_print(code: str) -> str:
            if not self.pretty_print:
                return code

            try:
                from black import FileMode, format_file_contents
                return format_file_contents(code, fast=True, mode=FileMode())
            except ImportError:
                check_source_language(
                    False,
                    'Black not available, not pretty-printing Python code',
                    severity=Severity.warning,
                    ok_for_codegen=True
                )
                return code

        def render_python_template(file_path: str,
                                   *args: Any,
                                   **kwargs: Any) -> None:
            with names.camel:
                code = ctx.render_template(*args, **kwargs)

            # If pretty-printing failed, write the original code anyway in
            # order to ease debugging.
            exc = None
            try:
                pp_code = pretty_print(code)
            except SyntaxError:
                pp_code = code

            write_source_file(file_path, pp_code, self.post_process_python)
            if exc:
                raise exc

        # Emit the Python modules themselves
        render_python_template(
            os.path.join(self.python_pkg_dir, '__init__.py'),
            'python_api/module_py',
            c_api=ctx.c_api_settings,
            pyapi=ctx.python_api_settings,
            module_name=ctx.python_api_settings.module_name
        )

        # Emit the empty "py.type" file so that users can easily leverage type
        # annotations in the generated bindings.
        write_source_file(os.path.join(self.python_pkg_dir, "py.typed"), "")

        # Emit the setup.py script to easily install the Python binding
        setup_py_file = os.path.join(self.lib_root, 'python', 'setup.py')
        write_source_file(
            setup_py_file,
            ctx.render_template('python_api/setup_py'),
            self.post_process_python
        )

    def emit_python_playground(self, ctx: CompileCtx) -> None:
        """
        Emit sources for the Python playground script.
        """
        playground_file = os.path.join(
            self.scripts_dir,
            '{}_playground'.format(ctx.short_name_or_long)
        )
        write_source_file(
            playground_file,
            ctx.render_template(
                'python_api/playground_py',
                module_name=ctx.python_api_settings.module_name
            ),
            self.post_process_python
        )
        os.chmod(playground_file, 0o775)

    def emit_gdb_helpers(self, ctx: CompileCtx) -> None:
        """
        Emit support files for GDB helpers.
        """
        lib_name = ctx.ada_api_settings.lib_name.lower()
        gdbinit_path = os.path.join(self.lib_root, 'gdbinit.py')
        gdb_c_path = os.path.join(self.src_dir, '{}-gdb.c'.format(lib_name))

        # Always emit the ".gdbinit.py" GDB script
        write_source_file(
            gdbinit_path,
            ctx.render_template(
                'gdb_py',
                langkit_path=os.path.dirname(os.path.dirname(__file__)),
                lib_name=lib_name,
                prefix=ctx.short_name_or_long,
            ),
            self.post_process_python
        )

        # Generate the C file to embed the absolute path to this script in the
        # generated library only if requested.
        if self.generate_gdb_hook:
            write_source_file(
                gdb_c_path,
                ctx.render_template('gdb_c', gdbinit_path=gdbinit_path,
                                    os_name=os.name),
                self.post_process_cpp
            )
            self.project_languages.add('C')

    def emit_ocaml_api(self, ctx: CompileCtx) -> None:
        """
        Generate binding for the external OCaml API.
        """
        if not ctx.ocaml_api_settings:
            return

        ctx.ocaml_api_settings.init_type_graph()

        if not os.path.isdir(self.ocaml_dir):
            os.mkdir(self.ocaml_dir)

        with names.camel:
            # Write an empty ocamlformat file so we can call ocamlformat
            write_source_file(
                os.path.join(self.ocaml_dir, '.ocamlformat'),
                ''
            )

            ctx = get_context()
            code = ctx.render_template(
                "ocaml_api/module_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings
            )

            ocaml_filename = '{}.ml'.format(ctx.c_api_settings.lib_name)
            write_ocaml_file(
                os.path.join(self.ocaml_dir, ocaml_filename),
                code,
                self.post_process_ocaml,
            )

            code = ctx.render_template(
                "ocaml_api/module_sig_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings
            )

            ocaml_filename = '{}.mli'.format(ctx.c_api_settings.lib_name)
            write_ocaml_file(
                os.path.join(self.ocaml_dir, ocaml_filename),
                code,
                self.post_process_ocaml,
            )

            # Emit dune file to easily compile and install bindings
            code = ctx.render_template(
                "ocaml_api/dune_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings
            )

            write_source_file(os.path.join(self.ocaml_dir, 'dune'), code)
            write_source_file(os.path.join(self.ocaml_dir, 'dune-project'),
                              '(lang dune 1.6)')

            # Write an empty opam file to install the lib with dune
            write_source_file(
                os.path.join(self.ocaml_dir,
                             '{}.opam'.format(ctx.c_api_settings.lib_name)),
                ''
            )

    def write_ada_module(self,
                         out_dir: str,
                         template_base_name: str,
                         qual_name: List[str],
                         has_body: bool = True,
                         cached_body: bool = False,
                         in_library: bool = False,
                         is_interface: bool = True) -> None:
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
            qual_name_str = '.'.join(qual_name)
            with_clauses = self.context.with_clauses[(qual_name_str, kind)]
            full_qual_name = [
                self.context.lib_name.camel_with_underscores
            ] + qual_name

            # When requested, register library module as library interfaces
            if is_interface and in_library:
                self.add_library_interface(ada_file_path(out_dir, kind,
                                                         full_qual_name),
                                           generated=True)

            # If asked not to generate the body, skip the rest
            if kind == AdaSourceKind.body and cached_body:
                return

            with names.camel_with_underscores:
                write_ada_file(
                    out_dir=out_dir,
                    source_kind=kind,
                    qual_name=full_qual_name,
                    content=self.context.render_template(
                        '{}{}_ada'.format(
                            template_base_name +
                            # If the base name ends with a /, we don't
                            # put a "_" separator.
                            ('' if template_base_name.endswith('/') else '_'),
                            kind.value
                        ),
                        with_clauses=with_clauses,
                    ),
                    post_process=self.post_process_ada
                )

        do_emit(AdaSourceKind.spec)
        if has_body:
            do_emit(AdaSourceKind.body)
