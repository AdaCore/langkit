"""
Test that using the ``domain`` DSL construct with bare nodes does not set
uninitialized data into the corresponding logic variable.
"""

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import Bind, Entity, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    v = UserField(type=T.LogicVar, public=False)


class Example(FooNode):
    @langkit_property(return_type=T.Example.entity)
    def identity():
        return Entity

    @langkit_property(public=True)
    def test():
        return (Self.v.domain([Self]) &
                Bind(Self.v, Self.v, conv_prop=Example.identity)).solve


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    gpr_mains=['main.adb'],
    types_from_lkt=True,
)
print('Done')
