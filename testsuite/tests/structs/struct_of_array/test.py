"""
Test that structs containing arrays work correctly.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import New, String, langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class KV(Struct):
    key = UserField(type=T.String)
    val = UserField(type=T.String)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def get():
        return New(T.KV, key=String("So"), val=String("What"))


class Example(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
build_and_run(g, py_script='main.py')
print('Done')
