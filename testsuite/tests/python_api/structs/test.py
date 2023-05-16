"""
Check that passing structs as property argument and returning structs works as
expected.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class MyStruct(Struct):
    a = UserField(type=T.Int)
    b = UserField(type=T.Int)


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def sum(s=T.MyStruct):
        return s.a + s.b

    @langkit_property(public=True)
    def create(a=T.Int, b=T.Int):
        return T.MyStruct.new(a=a, b=b)


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py",
              types_from_lkt=True)
print("")
print("Done")
