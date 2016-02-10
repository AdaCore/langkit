import os
import shutil

from langkit.compile_context import CompileCtx

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

    except AssertionError as exc:
        print('{}: {}'.format(
            type(exc).__name__,
            str(exc)
        ))

    else:
        print('Code generation was successful')
