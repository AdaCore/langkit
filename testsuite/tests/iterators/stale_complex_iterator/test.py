"""
Test the safety nets of iterators on a complex example, where node references
lie in a deeply nested structure.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import (
    ArrayLiteral, Entity, No, Var, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class TestStruct(Struct):
    count = UserField(type=T.Int)
    nodes = UserField(type=T.Example.entity.array)


class Example(FooNode):
    @langkit_property(public=True, return_type=T.TestStruct.iterator.iterator)
    def test_struct_iterator():
        val = Var(TestStruct.new(
            count=2,
            nodes=[No(Example.entity), Entity],
        ))
        itr = Var(ArrayLiteral(
            [val, val],
            element_type=TestStruct
        ).to_iterator)
        return ArrayLiteral(
            [itr, itr],
            element_type=T.TestStruct.iterator
        ).to_iterator


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py', ada_main='main.adb',
              lkt_semantic_checks=True)
print('Done')
