"""
Test that the .next_sibling and .previous_sibling properties work as expected.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Tuple(FooNode):
    id = Field()
    nodes = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
