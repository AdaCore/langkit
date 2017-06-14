from __future__ import absolute_import, division, print_function

from os import path

from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.dsl import ASTNode, LongType, Struct, UserField


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    pass


class StructA(Struct):
    a = UserField(type=LongType)

try:
    class StructB(StructA):
        b = UserField(type=LongType)
except DiagnosticError:
    pass

print('')
print('Done')
