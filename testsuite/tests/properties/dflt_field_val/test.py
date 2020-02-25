"""
Test that default values for struct fields have intended effects on New
expressions.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import New, No, langkit_property

from utils import build_and_run


class KV(Struct):
    key = UserField(type=T.String)
    value = UserField(type=T.Int, default_value=42)
    node = UserField(type=T.FooNode, default_value=No(T.FooNode))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def build_1(key=T.String):
        return New(T.KV, key=key)

    @langkit_property(public=True)
    def build_2(key=T.String, value=T.Int):
        return New(T.KV, key=key, value=value)


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
