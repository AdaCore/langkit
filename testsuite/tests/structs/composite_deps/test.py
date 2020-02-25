"""
Test rejection of dependency loops in composite types.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import New, No, langkit_property

from utils import emit_and_print_errors


class MyStruct(Struct):
    children = UserField(type=T.MyStruct.array)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def get():
        return New(T.MyStruct, children=No(T.MyStruct.array))


class Example(FooNode):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
