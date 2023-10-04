"""
Test PLE on a node which contains a very large number of children.
"""

from langkit.dsl import ASTNode

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    unparse_script=unparse_all_script,
    types_from_lkt=True,
)
print('Done')
