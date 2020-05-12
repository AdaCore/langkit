"""
Test that the "full_sloc_image" works as expected.
"""

from langkit.dsl import ASTNode
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
