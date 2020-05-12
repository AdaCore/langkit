"""
Test that getting a unit using different filenames for the same file return the
same unit (i.e. that the filename is canonicalized).
"""

from langkit.dsl import ASTNode, abstract
from langkit.expressions import Property

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class EnumNode(FooNode):
    prop = Property(True, public=True)


class HasExample(EnumNode):
    enum_node = True
    qualifier = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
