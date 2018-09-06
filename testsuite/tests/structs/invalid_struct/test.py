from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, Integer, Struct, UserField


class FooNode(ASTNode):
    pass


class StructA(Struct):
    a = UserField(type=Integer)

try:
    class StructB(StructA):
        b = UserField(type=Integer)
except DiagnosticError:
    pass

print('')
print('Done')
