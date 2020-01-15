"""
Test that default values for struct fields have intended effects on New
expressions.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import New, No, langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class KV(Struct):
    key = UserField(type=T.String)
    val = UserField(type=T.Int, default_value=42)
    node = UserField(type=T.FooNode, default_value=No(T.FooNode))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def build_1(key=T.String):
        return New(T.KV, key=key)

    @langkit_property(public=True)
    def build_2(key=T.String, val=T.Int):
        return New(T.KV, key=key, val=val)


class Example(FooNode):
    token_node = True


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
build_and_run(g, py_script='main.py')
print('Done')
