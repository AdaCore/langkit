"""
Check that the generated code for empty structs works as expected.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import (
    BigIntLiteral, CharacterLiteral, New, No, langkit_property
)


from utils import build_and_run


class FooNode(ASTNode):
    pass


class MyStruct(Struct):
    pass


class NonEmptyStruct(Struct):
    char_field = UserField(type=T.Character)
    my_struct_field = UserField(type=T.MyStruct)
    bigint_field = UserField(type=T.BigInt)


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def new_my_struct():
        return No(MyStruct)

    @langkit_property(public=True)
    def new_non_empty_struct():
        return New(NonEmptyStruct,
                   char_field=CharacterLiteral("X"),
                   my_struct_field=No(MyStruct),
                   bigint_field=BigIntLiteral(42))

    @langkit_property(public=True)
    def identity(s=T.MyStruct):
        return s

    @langkit_property(public=True)
    def get_char(s=T.NonEmptyStruct):
        return s.char_field

    @langkit_property(public=True)
    def get_big_int(s=T.NonEmptyStruct):
        return s.bigint_field


build_and_run(lkt_file="expected_concrete_syntax.lkt", gpr_mains=["main.c"])

print("Done")
