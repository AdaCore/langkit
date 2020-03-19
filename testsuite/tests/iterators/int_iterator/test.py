"""
Test iteration over a simple iterator from the public APIs.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import (
    ArrayLiteral, Self, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Int.array)
    def values_array():
        return ArrayLiteral([1, 2, 3], element_type=T.Int)

    @langkit_property(public=True, return_type=T.Int.iterator)
    def values_iterator():
        return Self.values_array.to_iterator


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py',
              ada_main='main.adb')
print('Done')
