from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode


class FooNode(ASTNode):
    pass


try:
    class OtherNode(ASTNode):
        pass
except DiagnosticError:
    pass

print('Done')
