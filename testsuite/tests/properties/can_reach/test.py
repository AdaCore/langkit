"""
Test the handling of analysis units in the properties DSL.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def can_reach(n=T.FooNode.entity, from_node=T.FooNode.entity):
        return n.node.can_reach(from_node.node)


class Example(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
