from langkit.compiled_types import (
    ASTNode, Field, LongType, Struct, root_grammar_class
)
from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, Row

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class StructA(Struct):
    a = Field(type=LongType)

try:
    class StructB(StructA):
        b = Field(type=LongType)
except DiagnosticError:
    pass

print ''
print 'Done'
