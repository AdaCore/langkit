"""
Test basic use of iterators in public APIs.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import (
    ArrayLiteral, BigIntLiteral, Entity, Self, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Int.array)
    def int_array():
        return ArrayLiteral([1, 2, 3], element_type=T.Int)

    @langkit_property(public=True, return_type=T.Int.iterator)
    def int_iterator():
        return Self.int_array.to_iterator

    @langkit_property(public=True, return_type=T.Int.iterator)
    def int_iterator_identity(it=T.Int.iterator):
        return it

    @langkit_property(public=True, return_type=T.BigInt.array)
    def bigint_array():
        return [BigIntLiteral(1), BigIntLiteral(2), BigIntLiteral(3)]

    @langkit_property(public=True, return_type=T.BigInt.iterator)
    def bigint_iterator():
        return Self.bigint_array.to_iterator

    @langkit_property(public=True, return_type=T.Example.entity.array)
    def entities_array():
        return [Entity, Entity, Entity]

    @langkit_property(public=True, return_type=T.Example.entity.iterator)
    def entities_iterator():
        return Entity.entities_array.to_iterator


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
)
print("Done")
