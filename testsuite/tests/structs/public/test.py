"""
Test that structure types are properly bound in public APIs.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import (ArrayLiteral, BigIntLiteral, New, Self,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class MyStruct(Struct):
    entity_field = UserField(type=T.FooNode)
    array_field = UserField(type=T.FooNode.entity.array)
    bigint_field = UserField(type=T.BigInt)


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def get_struct():
        return New(MyStruct,
                   entity_field=Self,
                   array_field=ArrayLiteral([
                       Self.cast(T.FooNode).as_bare_entity,
                       Self.parent.as_bare_entity]),
                   bigint_field=BigIntLiteral(10**100))

    @langkit_property(public=True)
    def struct_identity(s=MyStruct):
        return s


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("")
print("Done")
