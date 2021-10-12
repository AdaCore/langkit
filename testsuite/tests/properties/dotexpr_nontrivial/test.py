"""
Check that the lowering of non-trivial DotExpr expression in Lkt works as
expected.
"""

from langkit.dsl import ASTNode
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def identity():
        return Self


class Example(FooNode):

    @langkit_property(public=True)
    def foo():
        return Self.identity.parent.identity


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
