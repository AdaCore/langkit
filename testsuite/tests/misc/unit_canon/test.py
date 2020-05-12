"""
Test that getting a unit using different filenames for the same file return the
same unit (i.e. that the filename is canonicalized).
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
