"""
Test rejection of dependency loops in composite types.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import New, No, langkit_property
from langkit.parsers import Grammar, List

from utils import emit_and_print_errors


class MyStruct(Struct):
    children = UserField(type=T.MyStruct.array)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def get():
        return New(T.MyStruct, children=No(T.MyStruct.array))


class Example(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
emit_and_print_errors(g)
print('Done')
