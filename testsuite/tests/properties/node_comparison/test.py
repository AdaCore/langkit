"""
Test that comparing nodes works as expected in the DSL.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def before(n=T.FooNode):
        return Self < n

    @langkit_property(public=True)
    def before_or_equal(n=T.Example):
        return Self <= n


class Example(FooNode):
    pass


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
