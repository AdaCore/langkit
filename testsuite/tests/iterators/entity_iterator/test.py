"""
Test iteration over entities from public APIs.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import (
    ArrayLiteral, Entity, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Example.entity.array)
    def entities_array():
        return ArrayLiteral(
            [Entity, Entity, Entity],
            element_type=Example.entity
        )

    @langkit_property(public=True, return_type=T.Example.entity.iterator)
    def entities_iterator():
        return Entity.entities_array.to_iterator


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py', ada_main="main.adb")
print('Done')
