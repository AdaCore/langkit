"""
Test that the automatic (un)wrapping of bare nodes in the interface of public
properties works as expected.
"""

from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def compute(n=FooNode):
        return n


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('')
print('Done')
