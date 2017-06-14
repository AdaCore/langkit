from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.dsl import ASTNode, Struct
from langkit.envs import EnvSpec

from os import path


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


try:
    class StructA(Struct):
        env_spec = EnvSpec()
except DiagnosticError:
    pass

print('')
print('Done')
