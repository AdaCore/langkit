"""
Test that Bind works when binding entities, and using an equality property that
takes entities.
"""

from langkit.dsl import ASTNode, Int, LogicVar, T, UserField
from langkit.expressions import (AbstractProperty, Bind, Let, Property, Self,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    prop = AbstractProperty(runtime_check=True, type=Int, public=True)


class Literal(FooNode):
    token_node = True

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)
    var = UserField(LogicVar, public=False)

    @langkit_property(return_type=T.Literal.entity)
    def node():
        return Self.as_entity

    b = Property(Bind(Self.var, Self.a, Self.node))

    @langkit_property(public=True)
    def public_pro():
        return Let(lambda _=Self.b: Self.as_bare_entity)


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
