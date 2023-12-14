"""
Test that structs containing arrays work correctly.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import New, String, langkit_property

from utils import build_and_run


class KV(Struct):
    key = UserField(type=T.String)
    value = UserField(type=T.String)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def get():
        return New(T.KV, key=String("So"), value=String("What"))


class Example(FooNode):
    pass


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
