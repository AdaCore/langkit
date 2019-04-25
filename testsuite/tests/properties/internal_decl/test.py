from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode
from langkit.expressions import No, Property


class FooNode(ASTNode):
    pass


try:
    class BarNode(FooNode):
        _prop = Property(No(FooNode))
except DiagnosticError:
    pass

print('Done')
