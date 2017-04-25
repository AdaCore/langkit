from __future__ import absolute_import, division, print_function

from os import path

from langkit.compiled_types import ASTNode, root_grammar_class
from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.expressions import Property, No


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


try:
    class BarNode(FooNode):
        _prop = Property(No(FooNode))
except DiagnosticError:
    pass

print('Done')
