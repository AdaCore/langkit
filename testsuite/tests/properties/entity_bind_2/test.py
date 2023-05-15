"""
Test that Bind works when binding entities, and using an equality property that
takes entities.
"""

from langkit.dsl import ASTNode, LogicVar, T, UserField, abstract
from langkit.expressions import (AbstractKind, Bind, Entity, Self, ignore,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class RootNode(FooNode):

    var = UserField(LogicVar, public=False)

    @langkit_property(kind=AbstractKind.abstract, return_type=T.Equation)
    def xref_eq(arg1=T.FooNode.entity, arg2=T.FooNode.entity):
        pass

    @langkit_property(public=True)
    def solve_eq(arg1=T.FooNode.entity, arg2=T.FooNode.entity):
        return Self.xref_eq(arg1, arg2).solve


class Identifier(RootNode):
    token_node = True

    @langkit_property()
    def xref_eq(arg1=T.FooNode.entity, arg2=T.FooNode.entity):
        ignore(arg1, arg2)
        return Self.var.domain([Self])


class Literal(RootNode):
    token_node = True

    @langkit_property(return_type=T.Literal.entity)
    def conv():
        return Entity

    @langkit_property()
    def xref_eq(arg1=T.FooNode.entity, arg2=T.FooNode.entity):
        return (Self.var.domain([arg1]) &
                Bind(Self.var, arg2, conv_prop=Self.conv))


build_and_run(lkt_file="expected_concrete_syntax.lkt", gpr_mains=["main.adb"])
print("Done")
