import os
import shutil
import subprocess
import sys

from langkit.compile_context import CompileCtx
from langkit.compiled_types import ASTNode, StructMetaClass, root_grammar_class
from langkit.diagnostics import DiagnosticError
from langkit.expressions import Self
from langkit.libmanage import ManageScript

from lexer_example import foo_lexer


def prepare_context(grammar, main_rule_name='main_rule',
                    lexer=foo_lexer,
                    library_fields_all_public=False):
    """
    Create a compile context and prepare the build directory for code
    generation.

    :param langkit.parsers.Grammar grammar: The language grammar to use for
        this context.

    :param str main_rule_name: Name of the main gramma rule to use for this
        context.

    :param langkit.lexer.Lexer lexer: The language lexer to use for this
        context.

    :param bool library_fields_all_public: Whether private fields should be
        exported in code generation (they are not by default).
    """

    # Have a clean build directory
    if os.path.exists('build'):
        shutil.rmtree('build')
    os.mkdir('build')

    # Try to emit code
    ctx = CompileCtx(lang_name='Foo',
                     main_rule_name=main_rule_name,
                     lexer=lexer,
                     grammar=grammar)
    ctx.library_fields_all_public = library_fields_all_public

    return ctx


def emit_and_print_errors(grammar, main_rule_name='main_rule',
                          lexer=foo_lexer,
                          library_fields_all_public=False):
    """
    Compile and emit code for CTX. Return whether this was successful.

    :rtype: bool
    """
    ctx = prepare_context(grammar, main_rule_name, lexer,
                          library_fields_all_public)

    try:
        ctx.emit('build', generate_lexer=False)
        # ... and tell about how it went
    except DiagnosticError:
        # If there is a diagnostic error, don't say anything, the diagnostics
        # are enough.
        return False

    else:
        print 'Code generation was successful'
        return True


def build_and_run(grammar, py_script, main_rule_name='main_rule',
                  lexer=foo_lexer,
                  library_fields_all_public=False):
    """
    Compile and emit code for CTX and build the generated library. Then run
    PY_SCRIPT with this library available.

    An exception is raised if any step fails (the script must return code 0).
    """

    ctx = prepare_context(grammar, main_rule_name, lexer,
                          library_fields_all_public)

    class Manage(ManageScript):
        def create_context(self, args):
            return ctx

    m = Manage()

    # First build the library
    argv = ['-vnone', 'make']
    if ctx.library_fields_all_public:
        argv.append('--library-fields-all-public')
    m.run(argv)

    # Then execute a script with it
    subprocess.check_call(
        [sys.executable, py_script],
        env=m.derived_env()
    )


def reset_langkit():
    """
    Reset global state in Langkit.

    TODO: this is a hack to workaround another hack. At some point in the
    future, we should get rid of this global state in Langkit.
    """
    StructMetaClass.root_grammar_class = None
    StructMetaClass.astnode_types = [ASTNode]
    StructMetaClass.struct_types = []
    StructMetaClass.env_metadata = None
    Self.__dict__['_frozen'] = False
