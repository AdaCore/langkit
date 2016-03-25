import os
import shutil

from langkit.compile_context import CompileCtx
from langkit.compiled_types import root_grammar_class, StructMetaClass
from langkit.diagnostics import DiagnosticError
from langkit.expressions import Self

from lexer_example import foo_lexer


def emit_and_print_errors(grammar, main_rule_name='main_rule',
                          lexer=foo_lexer):
    # Have a clean build directory
    if os.path.exists('build'):
        shutil.rmtree('build')
    os.mkdir('build')

    # Try to emit code
    ctx = CompileCtx(lang_name='Foo',
                     main_rule_name=main_rule_name,
                     lexer=lexer,
                     grammar=grammar)

    try:
        ctx.emit('build', generate_lexer=False)
        # ... and tell about how it went
    except DiagnosticError:
        # If there is a diagnostic error, don't say anything, the diagnostics
        # are enough.
        pass

    else:
        print 'Code generation was successful'


def reset_langkit():
    """
    Reset global state in Langkit.

    TODO: this is a hack to workaround another hack. At some point in the
    future, we should get rid of this global state in Langkit.
    """
    StructMetaClass.root_grammar_class = None
    StructMetaClass.astnode_types = []
    StructMetaClass.struct_types = []
    StructMetaClass.env_metadata = None
    Self.__dict__['_frozen'] = False
