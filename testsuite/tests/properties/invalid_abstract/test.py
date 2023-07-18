import langkit
from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, T, abstract
from langkit.expressions import AbstractProperty

from utils import emit_and_print_errors


def run_unsafe(runtime_check, abstract_root_prop):
    if abstract_root_prop:
        class FooNode(ASTNode):
            root_prop = AbstractProperty(T.Bool, public=True)

        @abstract
        class BaseNode(FooNode):
            root_prop = AbstractProperty(T.Bool, runtime_check=True)

        class Number(BaseNode):
            pass

        class Identifier(BaseNode):
            pass

    else:
        class FooNode(ASTNode):
            pass

        @abstract
        class BaseNode(FooNode):
            prop = AbstractProperty(T.Bool, public=True)

        class Identifier(BaseNode):
            prop = AbstractProperty(T.Bool, runtime_check=runtime_check)

        class Number(BaseNode):
            prop = AbstractProperty(T.Bool, runtime_check=runtime_check)

    emit_and_print_errors(lkt_file='foo.lkt')


def run(name, runtime_check=False, abstract_root_prop=False):
    print('== {} =='.format(name))
    try:
        run_unsafe(runtime_check, abstract_root_prop)
    except DiagnosticError:
        pass
    finally:
        langkit.reset()
    print('')


run('Abstract overrides without runtime check', runtime_check=False)
run('Abstract overrides with runtime check', runtime_check=True)
run('Abstract in root not overriden in list', abstract_root_prop=True)
print('Done')
