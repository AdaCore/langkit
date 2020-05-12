from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode


class FooNode(ASTNode):
    pass


class TokenNode(FooNode):
    token_node = True

try:
    class NonTokenNode(TokenNode):
        token_node = False
except DiagnosticError:
    pass

print('Done')
