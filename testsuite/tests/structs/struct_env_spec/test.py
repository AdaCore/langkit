from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, Struct
from langkit.envs import EnvSpec


class FooNode(ASTNode):
    pass


try:
    class StructA(Struct):
        env_spec = EnvSpec()
except DiagnosticError:
    pass

print('')
print('Done')
