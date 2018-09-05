from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, IntegerType, Struct, UserField


class FooNode(ASTNode):
    pass


class StructA(Struct):
    a = UserField(type=IntegerType)

try:
    class StructB(StructA):
        b = UserField(type=IntegerType)
except DiagnosticError:
    pass

print('')
print('Done')
