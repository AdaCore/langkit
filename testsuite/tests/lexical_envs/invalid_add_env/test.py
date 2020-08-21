from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


try:
    class ExampleNode(FooNode):
        env_spec = EnvSpec(add_env(), add_env())
except DiagnosticError:
    pass
else:
    emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
