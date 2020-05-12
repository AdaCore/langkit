"""
Test that public UserField on nodes are rejected.
"""

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, UserField


class FooNode(ASTNode):
    pass


try:
    class Example(FooNode):
        f = UserField(FooNode)
except DiagnosticError:
    pass

print('Done')
