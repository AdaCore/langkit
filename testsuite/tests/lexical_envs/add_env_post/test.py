"""
Check that add_env is forbidden in post-node PLE.
"""

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env, handle_children


class FooNode(ASTNode):
    pass


try:
    class Example(FooNode):
        token_node = True

        env_spec = EnvSpec(
            handle_children(),
            add_env(),
        )
except DiagnosticError:
    pass

print('Done')
