"""
Check the absence of crash when Bind's conversion property (conv_prop) is
called on a mismatching node type.
"""

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.expressions import Bind, No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    lvar = UserField(T.LogicVar, public=False)


class Examples(FooNode):
    e1 = Field(type=T.RegularExample)
    e2 = Field(type=T.PlusExample)

    @langkit_property(public=True)
    def do_solving():
        return (
            Bind(Self.e1.lvar, Self.e2.lvar, conv_prop=PlusExample.conv_prop)
            & Self.e1.lvar.domain([Self.e1])
            & Self.e2.lvar.domain([Self.e2])
        ).solve


class RegularExample(FooNode):
    pass


class PlusExample(FooNode):

    @langkit_property()
    def conv_prop():
        return No(T.RegularExample.entity)


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
