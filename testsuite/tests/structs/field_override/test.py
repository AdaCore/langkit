"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import (
    ASTNode, Field, T, abstract, root_grammar_class
)
from langkit.diagnostics import DiagnosticError, Diagnostics
from langkit.expressions import Property, Self


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


def run():
    @root_grammar_class()
    class FooNode(ASTNode):
        pass

    @abstract
    class RootNode(FooNode):
        name = Property(Self.match(
            lambda e=T.Expr: e.name,
            lambda n=T.Name: n
        ))

    class Expr(RootNode):
        name = Field()

    class Name(RootNode):
        tok = Field()


try:
    run()
except DiagnosticError:
    pass
print('')
print('Done')
