from __future__ import annotations

import dataclasses
import os
import os.path as P
import shlex
import shutil
import subprocess
import sys
import traceback
from typing import List, Optional, Set

from langkit.compile_context import (
    CacheCollectionConf, CompileCtx, LibraryEntity, UnparseScript
)
from langkit.diagnostics import DiagnosticError, Diagnostics, WarningSet
from langkit.libmanage import ManageScript

from drivers.valgrind import valgrind_cmd


python_support_dir = P.dirname(P.abspath(__file__))
c_support_dir = P.join(python_support_dir, "..", "c_support")


Diagnostics.blacklisted_paths.append(python_support_dir)


default_warning_set = WarningSet()

# We don't want to be forced to provide dummy docs for nodes and public
# properties in testcases.
default_warning_set.disable(WarningSet.undocumented_nodes)
default_warning_set.disable(WarningSet.undocumented_public_properties)

project_template = """
with "libfoolang";

project Gen is
    for Languages use ({languages});
    for Source_Dirs use ({source_dirs});
    for Object_Dir use "obj";
    for Main use ({main_sources});

    package Compiler is
        for Default_Switches ("Ada") use
          ("-g", "-O0", "-gnata", "-gnatwae", "-gnatyg");
        for Default_Switches ("C") use
          ("-g", "-O0", "-Wall", "-W", "-Werror", "-pedantic");
    end Compiler;

    package Binder is
        for Switches ("Ada") use ("-Es");
    end Binder;
end Gen;
"""


@dataclasses.dataclass
class Main:
    source_file: str
    """
    Basename of the main source file.
    """

    args: list[str] = dataclasses.field(default_factory=list)
    """
    Arguments to pass to this main when running it.
    """

    @property
    def label(self) -> str:
        """
        Return a representation of this main that is suitable to include in
        test baselines.
        """
        return " ".join([self.source_file] + self.args)

    @property
    def unit_name(self) -> str:
        """
        Return the name of the main source file without extension.
        """
        return os.path.splitext(self.source_file)[0]

    @classmethod
    def parse(cls, value: str) -> Main:
        """
        Create a Main instance from a shell-encoded list of arguments.
        """
        argv = shlex.split(value)
        return cls(argv[0], argv[1:])


valgrind_enabled = bool(os.environ.get('VALGRIND_ENABLED'))
jobs = int(os.environ.get('LANGKIT_JOBS', '1'))


# Determine where to find the root directory for Langkit sources
langkit_root = os.environ.get('LANGKIT_ROOT_DIR')
if not langkit_root:
    test_dir = P.dirname(P.abspath(__file__))
    testsuite_dir = P.dirname(test_dir)
    langkit_root = P.dirname(testsuite_dir)


# When unparsing the concrete syntax, name of the file to write
unparse_destination = 'concrete_syntax.lkt'
unparse_script = ('to:{},import:lexer_example,grammar,nodes'
                  .format(unparse_destination))
unparse_all_script = 'to:{},lexer,grammar,nodes'.format(unparse_destination)


def prepare_context(grammar=None, lexer=None, lkt_file=None,
                    warning_set=default_warning_set,
                    default_unit_provider=None, symbol_canonicalizer=None,
                    show_property_logging=False, types_from_lkt=False,
                    version: Optional[str] = None,
                    build_date: Optional[str] = None,
                    standalone: bool = False,
                    property_exceptions: Set[str] = set(),
                    default_unparsing_config: str | None = None,
                    cache_coll_conf: Optional[CacheCollectionConf] = None):
    """
    Create a compile context and prepare the build directory for code
    generation.

    :param langkit.parsers.Grammar grammar: The language grammar to use for
        this context.

    :param langkit.lexer.Lexer lexer: The language lexer to use for this
        context.

    :param str|None lkt_file: If provided, file from which to read the Lkt
        language spec.

    :param WarningSet warning_set: Set of warnings to emit.

    :param langkit.compile_context.LibraryEntity|None default_unit_provider:
        Default unit provider to use for this context, if any.

    :param langkit.compile_context.LibraryEntity|None symbol_canonicalizer:
        Symbol canonicalizer to use for this context, if any.

    :param bool show_property_logging: See CompileCtx.show_property_logging.

    :param bool types_from_lkt: See CompileCtx.types_from_lkt.

    :param version: See CompileCtx's constructor.

    :param build_date: See CompileCtx's constructor.

    :param standalone: See CompileCtx's constructor.

    :param default_unparsing_config: See the homonym CompileCtx constructor
        argument.

    :param cache_coll_conf: See CompileCtx's ``cache_collection_conf``
        constructor argument.
    """

    # Have a clean build directory
    if P.exists('build'):
        shutil.rmtree('build')
    os.mkdir('build')

    # Try to emit code
    ctx = CompileCtx(
        lang_name='Foo', short_name='foo', lexer=lexer, grammar=grammar,
        default_unit_provider=default_unit_provider,
        symbol_canonicalizer=symbol_canonicalizer,
        show_property_logging=show_property_logging,
        lkt_file=lkt_file,
        types_from_lkt=types_from_lkt,
        version=version,
        build_date=build_date,
        standalone=standalone,
        property_exceptions=property_exceptions,
        default_unparsing_config=default_unparsing_config,
        cache_collection_conf=cache_coll_conf,
    )
    ctx.warnings = warning_set

    return ctx


def emit_and_print_errors(grammar=None, lexer=None, lkt_file=None,
                          warning_set=default_warning_set,
                          symbol_canonicalizer=None,
                          unparse_script=None,
                          version=None,
                          build_date=None,
                          explicit_passes_triggers={},
                          types_from_lkt: bool = False):
    """
    Compile and emit code the given set of arguments. Return the compile
    context if this was successful, None otherwise.

    :param langkit.parsers.Grammar grammar_fn: The language grammar to use.

    :param langkit.lexer.Lexer lexer: The lexer to use along with the grammar.
        Use `lexer_example.foo_lexer` if left to None.

    :param str|None lkt_file: If provided, file from which to read the Lkt
        language spec.

    :param WarningSet warning_set: Set of warnings to emit.

    :param langkit.compile_context.LibraryEntity|None symbol_canonicalizer:
        Symbol canoncalizes to use for this context, if any.

    :rtype: None|langkit.compile_context.CompileCtx

    :param None|str unparse_script: Script to unparse the language spec.

    :param version: See CompileCtx's constructor.

    :param build_date: See CompileCtx's constructor.

    :param types_from_lkt: See CompileCtx.types_from_lkt.

    :param property_exceptions: See CompileCtx's constructor.
    """

    try:
        ctx = prepare_context(
            grammar,
            lexer,
            lkt_file,
            warning_set,
            symbol_canonicalizer=symbol_canonicalizer,
            types_from_lkt=types_from_lkt,
            version=version,
            build_date=build_date,
        )
        ctx.create_all_passes(
            'build',
            unparse_script=(UnparseScript(unparse_script)
                            if unparse_script else None),
            explicit_passes_triggers=explicit_passes_triggers
        )
        ctx.emit()
        # ... and tell about how it went
    except DiagnosticError:
        # If there is a diagnostic error, don't say anything, the diagnostics
        # are enough.
        return None
    else:
        print('Code generation was successful')
        return ctx


def build_and_run(
    lkt_file: str,
    default_unparsing_config: str | None = None,
    default_unit_provider: LibraryEntity | None = None,
    symbol_canonicalizer: LibraryEntity | None = None,
    show_property_logging: bool = False,
    version: str | None = None,
    build_date: str | None = None,
    property_exceptions: Set[str] = set(),
    cache_collection_conf: Optional[CacheCollectionConf] = None,
    py_script: str | None = None,
    py_args: list[str] | None = None,
    gpr_mains: list[Main] | None = None,
    ocaml_main: Main | None = None,
    java_main: Main | None = None,
    ni_main: Main | None = None,
) -> None:
    """
    Compile and emit code for `ctx` and build the generated library. Then,
    execute the provided scripts/programs, if any.

    An exception is raised if any step fails (the script must return code 0).

    :param lkt_file: If provided, file from which to read the Lkt language
        spec.
    :param default_unparsing_config: See the homonym CompileCtx constructor
        argument.
    :param default_unit_provider: Default unit provider to use for this
        context, if any.
    :param symbol_canonicalizer: Symbol canonicalizer to use for this context,
        if any.
    :param show_property_logging: If true, any property that has been marked
        with tracing activated will be traced on stdout by default, without
        need for any config file.
    :param version: See CompileCtx's constructor.
    :param build_date: See CompileCtx's constructor.
    :param property_exceptions: See CompileCtx's constructor.
    :param cache_collection_conf: See CompileCtx's constructor.
    :param py_script: If not None, name of the Python script to run with the
        built library available.
    :param python_args: Arguments to pass to the Python interpreter when
        running a Python script.
    :param gpr_mains: If not None, list of name of mains (Ada and/or C) for the
        generated GPR file, to build and run with the generated library. Each
        main can be either a Main instance or a string (for the main source
        file basename, the main is run without arguments).
    :param ocaml_main: If not None, name of the OCaml source file to build and
        run with the built library available.
    :param java_main: If not None, name of the Java main sourec file to build
        and run with the Langkit Java lib through JNI.
    :param ni_main: If not None, name of the Java main sourec file to build
        and run with the Langkit Java lib through Native Image.
    """

    class Manage(ManageScript):
        def __init__(self, ctx):
            self._cached_context = ctx
            super().__init__(root_dir=os.getcwd())

        def create_context(self, args):
            return self._cached_context

    # The call to build_and_run in test.py scripts should never be considered
    # as being part of the DSL to create diagnostics.
    for frame in traceback.extract_stack():
        Diagnostics.blacklist_frame(frame)

    build_mode = 'dev'

    maven_exec = os.environ.get('MAVEN_EXECUTABLE')
    maven_repo = os.environ.get('MAVEN_LOCAL_REPO')

    ctx = prepare_context(
        grammar=None,
        lexer=None,
        lkt_file=lkt_file,
        types_from_lkt=True,
        default_unparsing_config=default_unparsing_config,
        default_unit_provider=default_unit_provider,
        symbol_canonicalizer=symbol_canonicalizer,
        show_property_logging=show_property_logging,
        version=version,
        build_date=build_date,
        property_exceptions=property_exceptions,
        cache_coll_conf=cache_collection_conf,
    )
    m = Manage(ctx)

    extensions_dir = P.abspath('extensions')
    if P.isdir(extensions_dir):
        ctx.extensions_dir = extensions_dir

    # First build the library. Forward all test.py's arguments to the libmanage
    # call so that manual testcase runs can pass "-g", for instance.
    argv = (
        ['make']
        + sys.argv[1:]
        + ['-vnone', f'-j{jobs}', "--full-error-traces"]
    )

    # If there is a Java main, enable the Java bindings building
    if java_main is not None or ni_main is not None:
        argv.append('--enable-java')
        if maven_exec:
            argv.append('--maven-executable')
            argv.append(maven_exec)
        if maven_repo:
            argv.append('--maven-local-repo')
            argv.append(maven_repo)
        if ni_main is not None and os.name == 'nt':
            argv.append('--generate-msvc-lib')

    argv.append('--build-mode={}'.format(build_mode))
    for w in WarningSet.available_warnings:
        argv.append(
            '-{}{}'.format('W' if w in ctx.warnings else 'w', w.name)
        )

    # No testcase uses the generated mains, so save time: never build them
    argv.append('--disable-all-mains')

    return_code = m.run_no_exit(argv)

    # Flush stdout and stderr, so that diagnostics appear deterministically
    # before the script/program output.
    sys.stdout.flush()
    sys.stderr.flush()

    if return_code != 0:
        raise DiagnosticError()

    # Write a "setenv" script to make developper investigation convenient
    with open('setenv.sh', 'w') as f:
        m.write_setenv(f)

    env = m.derived_env()

    def run(*argv, **kwargs):
        subp_env = kwargs.pop("env", env)
        valgrind = kwargs.pop('valgrind', False)
        suppressions = kwargs.pop('valgrind_suppressions', [])
        assert not kwargs

        if valgrind_enabled and valgrind:
            argv = valgrind_cmd(list(argv), suppressions)

        subprocess.check_call(argv, env=subp_env)

    if py_script is not None:
        # Run the Python script.
        #
        # Note that in order to use the generated library, we have to use the
        # special Python interpreter the testsuite provides us. See the
        # corresponding code in testsuite/drivers/python_driver.py.
        args = [os.environ['PYTHON_INTERPRETER']]
        if py_args:
            args.extend(py_args)

        # Also note that since Python 3.8, we need special PATH processing for
        # DLLs: see the path_wrapper.py script.
        args.append(P.join(python_support_dir, "path_wrapper.py"))

        args.append(py_script)
        run(*args)

    if gpr_mains:
        # Canonicalize mains to Main instances
        gpr_mains = [
            (Main(m) if isinstance(m, str) else m) for m in gpr_mains
        ]

        source_dirs = [".", c_support_dir]
        main_source_files = sorted(m.source_file for m in gpr_mains)

        # Detect languages based on the source files present in the test
        # directory.
        langs = set()
        for f in os.listdir("."):
            if any(f.endswith(ext) for ext in [".c", ".h"]):
                langs.add("C")
            if any(f.endswith(ext) for ext in [".adb", ".ads"]):
                langs.add("Ada")

        # Generate a project file to build the given mains. Do a static build
        # (the default) to improve the debugging experience.
        with open("gen.gpr", "w") as f:

            def fmt_str_list(strings: List[str]) -> str:
                return ", ".join(f'"{s}"' for s in strings)

            f.write(project_template.format(
                languages=fmt_str_list(langs),
                source_dirs=fmt_str_list(source_dirs),
                main_sources=fmt_str_list(main_source_files),
            ))
        run("gprbuild", "-Pgen", "-q", "-p")

        # Now run all mains. If there are more than one main to run, print a
        # heading before each one.
        for i, main in enumerate(gpr_mains):
            if i > 0:
                print("")
            if len(gpr_mains) > 1:
                print(f"== {main.label} ==")
            sys.stdout.flush()
            run(
                P.join("obj", main.unit_name),
                *main.args,
                valgrind=True,
                valgrind_suppressions=["gnat"],
            )

    if ocaml_main is not None:
        # Set up a Dune project
        with open('dune', 'w') as f:
            f.write("""
                (executable
                  (name {})
                  (flags (-w -9))
                  (libraries {}))
            """.format(ocaml_main.unit_name, ctx.c_api_settings.lib_name))
        with open('dune-project', 'w') as f:
            f.write('(lang dune 1.6)')

        # Build the ocaml executable
        run('dune', 'build', '--display', 'quiet', '--root', '.',
            './{}.exe'.format(ocaml_main.unit_name))

        # Run the ocaml executable
        run('./_build/default/{}.exe'.format(ocaml_main.unit_name),
            *ocaml_main.args,
            valgrind=True,
            valgrind_suppressions=['ocaml'])

    if java_main is not None:
        java_exec = P.realpath(P.join(
            env['JAVA_HOME'],
            'bin',
            'java'
        ))
        cmd = [
            java_exec,
            '-Dfile.encoding=UTF-8',
            f"-Djava.library.path={env['LD_LIBRARY_PATH']}",
        ]
        if 'graalvm' in env['JAVA_HOME']:
            cmd.append((
                '--add-opens=org.graalvm.truffle/com.oracle.truffle.api.'
                'strings=ALL-UNNAMED'
            ))
        cmd += [java_main.source_file] + java_main.args
        run(*cmd)

    if ni_main is not None:
        # Compile the Java tests
        javac_exec = P.realpath(P.join(
            env['JAVA_HOME'],
            'bin',
            'javac'
        ))
        run(javac_exec, '-encoding', 'utf8', ni_main.source_file)

        # Run native-image to compile the tests.  Building Java bindings does
        # not go through GPRbuild, so we must explicitly give access to the
        # generated C header.
        java_env = m.derived_env(direct_c_header=True)
        ni_exec = P.realpath(P.join(
            os.environ['GRAAL_HOME'],
            'bin',
            ('native-image.cmd' if os.name == 'nt' else 'native-image')
        ))
        class_path = os.path.pathsep.join([
            P.realpath('.'),
            env['CLASSPATH'],
        ])
        run(
            ni_exec,
            '-cp', class_path,
            '--no-fallback',
            '--macro:truffle',
            '-H:NumberOfThreads=1',
            '-H:+BuildOutputSilent',
            '-H:+ReportExceptionStackTraces',
            os.path.splitext(ni_main.source_file)[0],
            'main',
            env=java_env,
        )

        # Run the newly created main
        run(P.realpath('main'), *ni_main.args)


def indent(text: str, prefix: str = "  ") -> str:
    """
    Indent all lines in `text` with the given prefix.

    :param text: Text to indent.
    :param prefix: Indentation string.
    """
    return "\n".join(prefix + line for line in text.splitlines())
