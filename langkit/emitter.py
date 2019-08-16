"""
Code emission for Langkit-generated libraries.
"""

from __future__ import absolute_import, division, print_function

import ast
from distutils.spawn import find_executable
from glob import glob
from io import StringIO
import json
import os
from os import path
import subprocess

from funcy import keep

from langkit.caching import Cache
from langkit.compile_context import ADA_BODY, ADA_SPEC, get_context
from langkit.diagnostics import Severity, check_source_language
import langkit.names as names
from langkit.template_utils import add_template_dir
from langkit.utils import Colors, printcol


def copy_file(from_path, to_path):
    """
    Helper to copy a source file.

    Return whether the file has been updated.

    :param str from_path: Path of the file to copy.
    :param str to_path: Destination path.
    """
    with open(from_path, 'rb') as f:
        content = f.read()
    write_source_file(to_path, content)


def write_source_file(file_path, source, post_process=None):
    """
    Helper to write a source file.

    Return whether the file has been updated.

    :param str file_path: Path of the file to write.
    :param str source: Content of the file to write.
    :param post_process: If provided, callable used to transform the source
        file content just before writing it.
    :type post_process: None | (str) -> str

    :rtype: bool
    """
    context = get_context()
    if post_process:
        source = post_process(source)
    if (not os.path.exists(file_path) or
            context.emitter.cache.is_stale(file_path, source)):
        if context.verbosity.debug:
            printcol('Rewriting stale source: {}'.format(file_path),
                     Colors.OKBLUE)
        with open(file_path, 'wb') as f:
            f.write(source)
        return True
    return False


def write_cpp_file(file_path, source, post_process=None):
    """
    Helper to write a C/C++ source file.

    :param str file_path: Path of the file to write.
    :param str source: Content of the file to write.
    """
    if write_source_file(file_path, source, post_process):
        if find_executable('clang-format'):
            subprocess.check_call(['clang-format', '-i', file_path])


def write_ocaml_file(file_path, source, post_process=None):
    """
    Helper to write a OCaml source file.

    :param str file_path: Path of the file to write.
    :param str source: Content of the file to write.
    """
    if write_source_file(file_path, source, post_process):
        if find_executable('ocamlformat'):
            subprocess.check_call(['ocamlformat', '-i', file_path])


def ada_file_path(out_dir, source_kind, qual_name):
    """
    Return the name of the Ada file for the given unit name/kind.

    :param str out_dir: The complete path to the directory in which we want to
        write the file.
    :param str source_kind: One of the constants ADA_SPEC or ADA_BODY,
        determining whether the source is a spec or a body.
    :param list[names.Name] qual_name: The qualified name of the Ada spec/body,
        as a list of Name components.
    """
    assert source_kind in (ADA_SPEC, ADA_BODY)
    file_name = '{}.{}'.format('-'.join(n.lower for n in qual_name),
                               'ads' if source_kind == ADA_SPEC else 'adb')
    return os.path.join(out_dir, file_name)


def write_ada_file(out_dir, source_kind, qual_name, content,
                   post_process=None):
    """
    Helper to write an Ada file.

    :param out_dir: See ada_file_path.
    :param source_kind: See ada_file_path.
    :param qual_name: See ada_file_path.
    :param str content: The source content to write to the file.
    """
    file_path = ada_file_path(out_dir, source_kind, qual_name)

    # If there are too many lines, which triggers obscure debug info bugs,
    # strip empty lines.
    lines = content.splitlines()
    if len(lines) > 200000:
        content = '\n'.join(l for l in lines if l.strip())

    # TODO: no tool is able to pretty-print a single Ada source file
    write_source_file(file_path, content, post_process)


class Emitter(object):
    """
    Code and data holder for code emission.
    """

    def __init__(self, context, lib_root, extensions_dir,
                 main_source_dirs=set(), main_programs=set(),
                 no_property_checks=False, generate_astdoc=True,
                 generate_gdb_hook=True, pretty_print=False,
                 post_process_ada=None, post_process_cpp=None,
                 post_process_python=None):
        """
        Generate sources for the analysis library. Also emit a tiny program
        useful for testing purposes.

        :param str lib_root: Path of the directory in which the library should
            be generated.

        :param str|None extensions_dir: Directory to contain extensions for
            code generation. If None is provided, assume there is no extension.

        :param set[str] main_source_dirs: List of source directories to use in
            the project file for mains. Source directories must be relative to
            the mains project file directory (i.e. $BUILD/src).

        :param set[str] main_programs: List of names for programs to build in
            addition to the generated library. To each X program, there must be
            a X.adb source file in the $BUILD/src directory.

        :param bool no_property_checks: If True, do not emit safety checks in
            the generated code for properties. Namely, this disables null
            checks on field access.

        :param bool generate_astdoc: Whether to generate the HTML documentation
            for AST nodes, their fields and their properties.

        :param bool generate_gdb_hook: Whether to generate the
            ".debug_gdb_scripts" section. Good for debugging, but better to
            disable for releases.

        :param bool pretty_print: If true, pretty-print the generated sources.

        :param post_process_ada: Optional post-processing for generated Ada
            source code.
        :type post_process_ada: None|(str) -> str

        :param post_process_cpp: Optional post-processing for generated C++
            source code.
        :type post_process_cpp: None|(str) -> str

        :param post_process_python: Optional post-processing for generated
            Python source code.
        :type post_process_python: None|(str) -> str
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

        for dirpath in keep(self.context.template_lookup_extra_dirs):
            add_template_dir(dirpath)

        self.no_property_checks = no_property_checks
        self.generate_astdoc = generate_astdoc
        self.generate_gdb_hook = generate_gdb_hook
        self.pretty_print = pretty_print
        self.post_process_ada = post_process_ada
        self.post_process_cpp = post_process_cpp
        self.post_process_python = post_process_python

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

        # Paths for the various directories in which code is generated
        self.include_path = path.join(self.lib_root, 'include')
        self.src_path = path.join(self.lib_root, 'include', self.lib_name_low)
        self.lib_path = path.join(self.lib_root, 'lib')
        self.share_path = path.join(self.lib_root, 'share', self.lib_name_low)
        self.python_path = path.join(self.lib_root, 'python')
        self.ocaml_path = path.join(self.lib_root, 'ocaml')

        self.dfa_code = None
        """
        Holder for the data structures used to generate code for the lexer
        state machine (DFA). As an optimization, it is left to None if we
        decide not to generate it (i.e. when the already generated sources are
        up-to-date).

        :type: langkit.lexer.regexp.DFACodeGenHolder
        """

    def setup_directories(self, ctx):
        """
        Make sure the tree of directories needed for code generation exists.
        """
        if not path.exists(self.lib_root):
            os.mkdir(self.lib_root)

        for d in ['include',
                  'include/{}'.format(self.lib_name_low),
                  'share',
                  'share/{}'.format(self.lib_name_low),
                  'obj', 'src', 'bin',
                  'lib', 'lib/gnat']:
            p = path.join(self.lib_root, d)
            if not path.exists(p):
                os.mkdir(p)

        if not path.exists(self.python_path):
            os.mkdir(self.python_path)

    def emit_lib_project_file(self, ctx):
        """
        Emit a project file for the generated library.
        """
        main_project_file = os.path.join(
            self.lib_path, 'gnat', '{}.gpr'.format(self.lib_name_low),
        )
        write_source_file(
            main_project_file,
            ctx.render_template(
                'project_file',
                lib_name=ctx.ada_api_settings.lib_name,
                os_path=os.path,
            )
        )

    def emit_astdoc(self, ctx):
        """
        If requested, generate the HTML documentation for node types.
        """
        if not self.generate_astdoc:
            return

        from langkit import astdoc

        f = StringIO()
        astdoc.write_astdoc(ctx, f)
        f.seek(0)
        write_source_file(os.path.join(self.share_path, 'ast-types.html'),
                          f.read())

    def generate_lexer_dfa(self, ctx):
        """
        Generate code for the lexer state machine.
        """
        # Source file that contains the state machine implementation
        lexer_sm_body = ada_file_path(
            self.src_path, ADA_BODY,
            [ctx.lib_name, names.Name('Lexer_State_Machine')])

        # Generate the lexer state machine iff the file is missing or its
        # signature has changed since last time.
        stale_lexer_spec = write_source_file(
            os.path.join(
                self.lib_root, 'obj',
                '{}_lexer_signature.txt'
                .format(ctx.short_name_or_long.lower)),
            json.dumps(ctx.lexer.signature, indent=2)
        )
        if not os.path.exists(lexer_sm_body) or stale_lexer_spec:
            self.dfa_code = ctx.lexer.build_dfa_code(ctx)

    def emit_ada_lib(self, ctx):
        """
        Emit Ada sources for the generated library.
        """

        ada_modules = [
            # Top (pure) package
            ('pkg_main', '', False),
            # Unit for initialization primitives
            ('pkg_init', 'Init', True),
            # Unit for declarations used by Analysis and Implementation
            ('pkg_common', 'Common', True),
            # Unit for public analysis primitives
            ('pkg_analysis', 'Analysis', True),
            # Unit for converters between public Ada types and C API-level ones
            ('pkg_c', 'C', True),
            # Unit for converters between public and implementation types
            ('pkg_converters', 'Converters', False),
            # Unit for implementation of analysis primitives
            ('pkg_implementation', 'Implementation', True),
            # Unit for AST introspection public API
            ('pkg_introspection', 'Introspection', True),
            # Unit for AST introspection internal API
            ('pkg_introspection_impl', 'Introspection_Implementation', True),
            # Unit for AST node iteration primitives
            ('pkg_iterators', 'Iterators', True),
            # Unit for AST rewriting primitives
            ('pkg_rewriting', 'Rewriting', True),
            # Unit for AST rewriting implementation
            ('pkg_rewriting_impl', 'Rewriting_Implementation', True),
            # Unit for AST unparsing primitives
            ('pkg_unparsing', 'Unparsing', True),
            # Unit for AST implementation of unparsing primitives
            ('pkg_unparsing_impl', 'Unparsing_Implementation', True),
            # Unit for all parsers
            ('parsers/pkg_main', 'Parsers', True),
            # Units for the lexer
            ('pkg_lexer', 'Lexer', True),
            ('pkg_lexer_impl', 'Lexer_Implementation', True),
            ('pkg_lexer_state_machine', 'Lexer_State_Machine',
             bool(self.dfa_code)),
            # Unit for debug helpers
            ('pkg_debug', 'Debug', True),
        ]

        for template_base_name, qual_name, has_body in ada_modules:
            qual_name = ([names.Name(n) for n in qual_name.split('.')]
                         if qual_name else [])
            self.write_ada_module(self.src_path, template_base_name, qual_name,
                                  has_body)

        # Add any sources in $lang_path/extensions/support if it exists
        if ctx.ext('support'):
            for f in glob(path.join(ctx.ext('support'), '*.ad*')):
                copy_file(f, self.src_path)

    def emit_mains(self, ctx):
        """
        Emit sources and the project file for mains.
        """
        with names.camel_with_underscores:
            write_ada_file(
                path.join(self.lib_root, 'src'),
                ADA_BODY, [names.Name('Parse')],
                ctx.render_template('main_parse_ada'),
                self.post_process_ada
            )

        imain_project_file = os.path.join(self.lib_root, 'src', 'mains.gpr')
        write_source_file(
            imain_project_file,
            ctx.render_template(
                'mains_project_file',
                lib_name=ctx.ada_api_settings.lib_name,
                source_dirs=self.main_source_dirs,
                main_programs=self.main_programs
            )
        )

    def emit_c_api(self, ctx):
        """
        Generate header and binding body for the external C API.
        """
        def render(template_name):
            return ctx.render_template(template_name)

        with names.lower:
            write_cpp_file(
                path.join(
                    self.include_path,
                    '{}.h'.format(ctx.c_api_settings.lib_name)),
                render('c_api/header_c'),
                self.post_process_cpp
            )

        self.write_ada_module(
            self.src_path, 'c_api/pkg_main',
            [names.Name(n) for n in 'Implementation.C'.split('.')]
        )

    def emit_python_api(self, ctx):
        """
        Generate the Python binding module.
        """
        package_dir = os.path.join(
            self.python_path,
            ctx.python_api_settings.module_name)
        if not os.path.isdir(package_dir):
            os.mkdir(package_dir)

        def strip_white_lines(code):
            tree = ast.parse(code)
            # Create an assoc of lines to a boolean flag indicating whether the
            # line is in a multiline string literal or not.
            lines = [[l, False] for l in code.splitlines()]

            # Find all the strings in the AST
            for s in (a for a in ast.walk(tree) if isinstance(a, ast.Str)):
                end_line = s.lineno
                start_line = end_line - len(s.s.splitlines()) + 1
                for l in range(start_line + 1, end_line):
                    lines[l - 1][1] = True

            return '\n'.join(
                l[0] for l in lines
                if (not all(c.isspace() for c in l[0])) or l[1]
            )

        def pretty_print(code):
            if not self.pretty_print:
                return code

            try:
                from yapf.yapflib.yapf_api import FormatCode
                return FormatCode(code)[0]
            except ImportError:
                check_source_language(
                    False,
                    'Yapf not available, using autopep8 to pretty-print'
                    ' Python code',
                    severity=Severity.warning,
                    ok_for_codegen=True
                )

            try:
                from autopep8 import fix_code
                return fix_code(code)
            except ImportError:
                check_source_language(
                    False,
                    'autopep8 not available, cannot pretty-print Python code',
                    severity=Severity.warning,
                    ok_for_codegen=True
                )
                return code

        with names.camel:
            code = ctx.render_template(
                'python_api/module_py',
                c_api=ctx.c_api_settings,
                pyapi=ctx.python_api_settings,
                module_name=ctx.python_api_settings.module_name
            )

            # If pretty-printing failed, write the original code anyway in
            # order to ease debugging.
            exc = None
            try:
                pp_code = pretty_print(strip_white_lines(code))
            except SyntaxError as exc:
                pp_code = code

            write_source_file(os.path.join(package_dir, '__init__.py'),
                              pp_code,
                              self.post_process_python)
            if exc:
                raise exc

        # Emit the setup.py script to easily install the Python binding
        setup_py_file = os.path.join(self.lib_root, 'python', 'setup.py')
        write_source_file(
            setup_py_file,
            ctx.render_template('python_api/setup_py'),
            self.post_process_python
        )

    def emit_python_playground(self, ctx):
        """
        Emit sources for the Python playground script.
        """
        playground_file = os.path.join(self.lib_root, 'bin', 'playground')
        write_source_file(
            playground_file,
            ctx.render_template(
                'python_api/playground_py',
                module_name=ctx.python_api_settings.module_name
            ),
            self.post_process_python
        )
        os.chmod(playground_file, 0o775)

    def emit_gdb_helpers(self, ctx):
        """
        Emit support files for GDB helpers.
        """
        gdbinit_path = os.path.join(self.lib_root, 'gdbinit.py')

        # Always emit the ".gdbinit.py" GDB script
        lib_name = ctx.ada_api_settings.lib_name.lower()
        write_source_file(
            gdbinit_path,
            ctx.render_template(
                'gdb_py',
                langkit_path=os.path.dirname(os.path.dirname(__file__)),
                lib_name=lib_name,
                prefix=ctx.short_name_or_long.lower,
            ),
            self.post_process_python
        )

        # Generate the C file to embed the absolute path to this script in the
        # generated library only if requested.
        if self.generate_gdb_hook:
            write_source_file(
                os.path.join(self.src_path, 'gdb.c'),
                ctx.render_template('gdb_c', gdbinit_path=gdbinit_path,
                                    os_name=os.name),
                self.post_process_cpp
            )

    def emit_ocaml_api(self, ctx):
        """
        Generate binding for the external OCaml API.

        :param str ocaml_path: The directory in which the OCaml module will
            be generated.
        """
        if not ctx.ocaml_api_settings:
            return

        if not os.path.isdir(self.ocaml_path):
            os.mkdir(self.ocaml_path)

        with names.camel:
            ctx = get_context()
            code = ctx.render_template(
                "ocaml_api/module_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings
            )

            ocaml_filename = '{}.ml'.format(ctx.c_api_settings.lib_name)
            write_ocaml_file(
                os.path.join(self.ocaml_path, ocaml_filename),
                code
            )

            code = ctx.render_template(
                "ocaml_api/module_sig_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings
            )

            ocaml_filename = '{}.mli'.format(ctx.c_api_settings.lib_name)
            write_ocaml_file(
                os.path.join(self.ocaml_path, ocaml_filename),
                code
            )

            # Emit dune file to easily compile and install bindings
            code = ctx.render_template(
                "ocaml_api/dune_ocaml",
                c_api=ctx.c_api_settings,
                ocaml_api=ctx.ocaml_api_settings
            )

            write_source_file(os.path.join(self.ocaml_path, 'dune'), code)
            write_source_file(os.path.join(self.ocaml_path, 'dune-project'),
                              '(lang dune 1.6)')

            # Write an empty opam file to install the lib with dune
            write_source_file(
                os.path.join(self.ocaml_path,
                             '{}.opam'.format(ctx.c_api_settings.lib_name)),
                ''
            )

    def write_ada_module(self, out_dir, template_base_name, qual_name,
                         has_body=True):
        """
        Write an Ada module (both spec and body) using a standardized scheme
        for finding the corresponding templates.

        :param str out_dir: The out directory for the generated module.

        :param str template_base_name: The base name for the template,
            basically everything that comes before the _body_ada/_spec_ada
            component, including the directory.

        :param list[names.Name] qual_name: Qualified name for the Ada module,
            as a list of "simple" package names. The base library name is
            automatically prepended to that list, so every generated module
            will be a child module of the base library module.

        :param bool has_body: If true, generate a body for this unit.
        """
        for kind in [ADA_SPEC] + ([ADA_BODY] if has_body else []):
            qual_name_str = '.'.join(n.camel_with_underscores
                                     for n in qual_name)
            with_clauses = self.context.with_clauses[(qual_name_str, kind)]
            with names.camel_with_underscores:
                write_ada_file(
                    out_dir=out_dir,
                    source_kind=kind,
                    qual_name=[self.context.lib_name] + qual_name,
                    content=self.context.render_template(
                        '{}{}_ada'.format(
                            template_base_name +
                            # If the base name ends with a /, we don't
                            # put a "_" separator.
                            ('' if template_base_name.endswith('/') else '_'),
                            kind
                        ),
                        with_clauses=with_clauses,
                    ),
                    post_process=self.post_process_ada
                )
