from langkit.compiled_types import ASTNode, Struct, root_grammar_class
from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.envs import EnvSpec

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


try:
    class StructA(Struct):
        env_spec = EnvSpec()
except DiagnosticError:
    pass

print ''
print 'Done'
