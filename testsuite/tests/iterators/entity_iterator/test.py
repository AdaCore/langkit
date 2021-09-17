"""
Test iteration over entities from public APIs.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Entity, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Example.entity.array)
    def entities_array():
        return [Entity, Entity, Entity]

    @langkit_property(public=True, return_type=T.Example.entity.iterator)
    def entities_iterator():
        return Entity.entities_array.to_iterator


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py', ada_main="main.adb",
              lkt_semantic_checks=True)
print('Done')
