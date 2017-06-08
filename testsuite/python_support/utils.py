from __future__ import absolute_import, division, print_function

import os
import shutil
import subprocess
import sys

from langkit.compile_context import CompileCtx
from langkit.compiled_types import CompiledTypeMetaclass, StructMetaclass
from langkit.diagnostics import DiagnosticError
from langkit.dsl import _StructMetaclass
from langkit.expressions import Self
from langkit.libmanage import ManageScript
from langkit.utils import reset_memoized


def prepare_context(grammar,
                    lexer=None,
                    library_fields_all_public=False):
    """
    Create a compile context and prepare the build directory for code
    generation.

    :param langkit.parsers.Grammar grammar: The language grammar to use for
        this context.

    :param langkit.lexer.Lexer lexer: The language lexer to use for this
        context.

    :param bool library_fields_all_public: Whether private fields should be
        exported in code generation (they are not by default).
    """

    if lexer is None:
        from lexer_example import foo_lexer
        lexer = foo_lexer

    # Have a clean build directory
    if os.path.exists('build'):
        shutil.rmtree('build')
    os.mkdir('build')

    # Try to emit code
    ctx = CompileCtx(lang_name='Foo',
                     lexer=lexer,
                     grammar=grammar)
    ctx.library_fields_all_public = library_fields_all_public

    return ctx


def emit_and_print_errors(grammar_fn,
                          lexer=None,
                          library_fields_all_public=False):
    """
    Compile and emit code for CTX. Return whether this was successful.

    :param () -> langkit.parsers.Grammar grammar_fn: A function returning the
        grammar to be used.

    :param langkit.lexer.Lexer lexer: The lexer to use along with the grammar.

    :param bool library_fields_all_public: Whether private fields should be
        exported in code generation (they are not by default).
    :rtype: bool
    """

    if lexer is None:
        from lexer_example import foo_lexer
        lexer = foo_lexer

    try:
        ctx = prepare_context(grammar_fn(), lexer, library_fields_all_public)
        ctx.emit('build', generate_lexer=False)
        # ... and tell about how it went
    except DiagnosticError:
        # If there is a diagnostic error, don't say anything, the diagnostics
        # are enough.
        return False
    else:
        print('Code generation was successful')
        return True
    finally:
        reset_langkit()


def build_and_run(grammar, py_script,
                  lexer=None,
                  library_fields_all_public=False):
    """
    Compile and emit code for CTX and build the generated library. Then run
    PY_SCRIPT with this library available.

    An exception is raised if any step fails (the script must return code 0).
    """

    if lexer is None:
        from lexer_example import foo_lexer
        lexer = foo_lexer

    ctx = prepare_context(grammar, lexer, library_fields_all_public)

    class Manage(ManageScript):
        def create_context(self, args):
            return ctx

    m = Manage()

    extensions_dir = os.path.abspath('extensions')
    if os.path.isdir(extensions_dir):
        ctx.extensions_dir = extensions_dir

    # First build the library. Forward all test.py's arguments to the libmanage
    # call so that manual testcase runs can pass "-g", for instance.
    argv = sys.argv[1:] + ['--full-error-traces', '-vnone', 'make']
    if ctx.library_fields_all_public:
        argv.append('--library-fields-all-public')
    m.run(argv)

    # Write a "setenv" script to make developper investigation convenient
    with open('setenv.sh', 'w') as f:
        m.write_setenv(f)

    # Then execute a script with it. Note that in order to use the generated
    # library, we have to use the special Python interpreter the testsuite
    # provides us. See the corresponding code in
    # testuite_support/python_driver.py.
    python_interpreter = os.environ['PYTHON_INTERPRETER']
    subprocess.check_call(
        [python_interpreter, py_script],
        env=m.derived_env()
    )


def reset_langkit():
    """
    Reset global state in Langkit.

    TODO: this is a hack to workaround another hack. At some point in the
    future, we should get rid of this global state in Langkit.
    """
    StructMetaclass.root_grammar_class = None
    StructMetaclass.astnode_types = []
    StructMetaclass.struct_types = []
    StructMetaclass.env_metadata = None
    StructMetaclass.entity_info = None
    Self.unfreeze()

    CompiledTypeMetaclass.types = []
    CompiledTypeMetaclass.type_dict = {}

    _StructMetaclass.reset()

    reset_memoized()
