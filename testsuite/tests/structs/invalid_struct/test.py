from __future__ import absolute_import, division, print_function

from langkit.compiled_types import (
    ASTNode, Field, LongType, Struct, root_grammar_class
)
from langkit.diagnostics import DiagnosticError, Diagnostics

from os import path


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class StructA(Struct):
    a = Field(type=LongType)

try:
    class StructB(StructA):
        b = Field(type=LongType)
except DiagnosticError:
    pass

print('')
print('Done')
