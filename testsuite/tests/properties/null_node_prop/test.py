"""
Test that calling a property on a null node literal generates valid Ada code.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import No, langkit_property

from utils import build


class FooNode(ASTNode):

    @langkit_property(public=True)
    def eval_unit():
        return No(T.FooNode).prop

    @langkit_property()
    def prop():
        return True


class Example(FooNode):
    pass


build(lkt_file='expected_concrete_syntax.lkt', types_from_lkt=True)
print('Done')
