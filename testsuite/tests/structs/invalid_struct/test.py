from __future__ import absolute_import, division, print_function

from langkit.diagnostics import DiagnosticError
from langkit.dsl import ASTNode, Int, Struct, UserField


class FooNode(ASTNode):
    pass


class StructA(Struct):
    a = UserField(type=Int)

try:
    class StructB(StructA):
        b = UserField(type=Int)
except DiagnosticError:
    pass

print('')
print('Done')
