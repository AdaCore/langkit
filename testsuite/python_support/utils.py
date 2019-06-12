from __future__ import absolute_import, division, print_function

import os
import os.path
import shutil
import subprocess
import sys

import langkit
import langkit.compile_context
from langkit.compile_context import CompileCtx
from langkit.diagnostics import DiagnosticError, WarningSet
from langkit.libmanage import ManageScript

from testsuite_support.valgrind import valgrind_cmd


default_warning_set = WarningSet()

# We don't want to be forced to provide dummy docs for nodes and public
# properties in testcases.
default_warning_set.disable(WarningSet.undocumented_nodes)
default_warning_set.disable(WarningSet.undocumented_public_properties)

pretty_print = bool(int(os.environ.get('LANGKIT_PRETTY_PRINT', '0')))

project_template = """
with "libfoolang";

project Gen is
    for Languages use ("Ada");
    for Source_Dirs use (".");
    for Object_Dir use "obj";
    for Main use ({main_sources});

    package Compiler is
        for Default_Switches ("Ada") use
          ("-g", "-O0", "-gnata", "-gnatwae", "-gnatyg");
    end Compiler;
end Gen;
"""


# Determine where to find the root directory for Langkit sources
langkit_root = os.environ.get('LANGKIT_ROOT_DIR')
if not langkit_root:
    test_dir = os.path.dirname(os.path.abspath(__file__))
    testsuite_dir = os.path.dirname(test_dir)
    langkit_root = os.path.dirname(testsuite_dir)


def prepare_context(grammar, lexer=None, warning_set=default_warning_set,
                    symbol_canonicalizer=None):
    """
    Create a compile context and prepare the build directory for code
    generation.

    :param langkit.parsers.Grammar grammar: The language grammar to use for
        this context.

    :param langkit.lexer.Lexer lexer: The language lexer to use for this
        context.

    :param WarningSet warning_set: Set of warnings to emit.

    :param langkit.compile_context.LibraryEntity|None symbol_canonicalizer:
        Symbol canoncalizes to use for this context, if any.
    """

    if lexer is None:
        from lexer_example import foo_lexer
        lexer = foo_lexer

    # Have a clean build directory
    if os.path.exists('build'):
        shutil.rmtree('build')
    os.mkdir('build')

    # Try to emit code
    ctx = CompileCtx(lang_name='Foo', lexer=lexer, grammar=grammar,
                     symbol_canonicalizer=symbol_canonicalizer)
    ctx.warnings = warning_set
    ctx.pretty_print = pretty_print

    return ctx


def emit_and_print_errors(grammar, lexer=None,
                          warning_set=default_warning_set,
                          generate_unparser=False, symbol_canonicalizer=None):
    """
    Compile and emit code for CTX. Return whether this was successful.

    :param langkit.parsers.Grammar grammar_fn: The language grammar to use.

    :param langkit.lexer.Lexer lexer: The lexer to use along with the grammar.
        Use `lexer_example.foo_lexer` if left to None.

    :param WarningSet warning_set: Set of warnings to emit.

    :param bool generate_unparser: Whether to generate unparser.

    :param langkit.compile_context.LibraryEntity|None symbol_canonicalizer:
        Symbol canoncalizes to use for this context, if any.

    :rtype: bool
    """

    if lexer is None:
        from lexer_example import foo_lexer
        lexer = foo_lexer

    try:
        ctx = prepare_context(grammar, lexer, warning_set,
                              symbol_canonicalizer=symbol_canonicalizer)
        ctx.emit('build', generate_unparser=generate_unparser)
        # ... and tell about how it went
    except DiagnosticError:
        # If there is a diagnostic error, don't say anything, the diagnostics
        # are enough.
        return False
    else:
        print('Code generation was successful')
        return True
    finally:
        lexer._dfa_code = None
        langkit.reset()


def build(grammar, lexer=None, warning_set=default_warning_set):
    """
    Shortcut for `build_and_run` to only build.
    """
    build_and_run(grammar, lexer=lexer, warning_set=warning_set)


def build_and_run(grammar, py_script=None, ada_main=None, lexer=None,
                  ocaml_main=None, warning_set=default_warning_set,
                  generate_unparser=False, symbol_canonicalizer=None):
    """
    Compile and emit code for `ctx` and build the generated library. Then,
    execute the provided scripts/programs, if any.

    An exception is raised if any step fails (the script must return code 0).

    :param langkit.lexer.Lexer lexer: The lexer to use along with the grammar.
        See emit_and_print_errors.
    :param None|str py_script: If not None, name of the Python script to run
        with the built library available.
    :param None|str|list[str] ada_main: If not None, list of name of main
        source files for Ada programs to build and run with the generated
        library. If the input is a single string, consider it's a single mail
        source file.
    :param WarningSet warning_set: Set of warnings to emit.
    :param bool generate_unparser: Whether to generate unparser.
    :param langkit.compile_context.LibraryEntity|None symbol_canonicalizer:
        Symbol canoncalizes to use for this context, if any.
    """

    if lexer is None:
        from lexer_example import foo_lexer
        lexer = foo_lexer

    ctx = prepare_context(grammar, lexer, warning_set,
                          symbol_canonicalizer=symbol_canonicalizer)

    class Manage(ManageScript):
        def create_context(self, args):
            return ctx

    m = Manage(override_lang_source_dir=False)

    extensions_dir = os.path.abspath('extensions')
    if os.path.isdir(extensions_dir):
        ctx.extensions_dir = extensions_dir

    # First build the library. Forward all test.py's arguments to the libmanage
    # call so that manual testcase runs can pass "-g", for instance.
    argv = sys.argv[1:] + ['--full-error-traces', '-vnone', 'make']
    for w in WarningSet.available_warnings:
        argv.append('-{}{}'.format('W' if w in warning_set else 'w', w.name))
    if not pretty_print:
        argv.append('--no-pretty-print')
    if generate_unparser:
        argv.append('--generate-unparser')
    m.run(argv)

    # Flush stdout and stderr, so that diagnostics appear deterministically
    # before the script/program output.
    sys.stdout.flush()
    sys.stderr.flush()

    # Write a "setenv" script to make developper investigation convenient
    with open('setenv.sh', 'w') as f:
        m.write_setenv(f)

    env = m.derived_env()

    def run(*argv):
        subprocess.check_call(argv, env=env)

    if py_script is not None:
        # Run the Python script. Note that in order to use the generated
        # library, we have to use the special Python interpreter the testsuite
        # provides us. See the corresponding code in
        # testuite_support/python_driver.py.
        python_interpreter = os.environ['PYTHON_INTERPRETER']
        run(python_interpreter, py_script)

    if ada_main is not None:
        if isinstance(ada_main, str):
            ada_main = [ada_main]

        # Generate a project file to build the given Ada main and then run
        # the program.
        with open('gen.gpr', 'w') as f:
            f.write(project_template.format(
                main_sources=', '.join('"{}"'.format(m) for m in ada_main)
            ))
        run('gprbuild', '-Pgen', '-q', '-p',
            '-XLIBRARY_TYPE=relocatable', '-XXMLADA_BUILD=relocatable')

        for i, m in enumerate(ada_main):
            assert m.endswith('.adb')
            if i > 0:
                print('')
            if len(ada_main) > 1:
                print('== {} =='.format(m))
            sys.stdout.flush()
            argv = [os.path.join('obj', m[:-4])]
            if os.environ.get('VALGRIND_ENABLED'):
                argv = valgrind_cmd(argv)
            run(*argv)

    if ocaml_main is not None:
        run('dune', 'exec', '--display', 'quiet', '--root', '.',
            './{}.exe'.format(ocaml_main))


def add_gpr_path(dirname):
    """
    Prepend the given directory name to the ``GPR_PROJECT_PATH`` environment
    variable.
    """
    old_path = os.environ.get('GPR_PROJECT_PATH')
    os.environ['GPR_PROJECT_PATH'] = (
        '{}{}{}'.format(dirname, os.path.pathsep, old_path)
        if old_path else dirname)
