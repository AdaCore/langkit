"""
Test that calling a memoized property on a null node works as expected:
memoization is inactive (no access to the memoization table), but the property
execution runs as if the property was not memoized (i.e. possibly no
PropertyError).
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, UserField
from langkit.expressions import (Bind, If, Not, Predicate, Self,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):

    # Create two predicates: one that can work when called on a null node
    # (pred1) and one that will crash (pred2).

    @langkit_property(memoized=True)
    def pred1():
        return Not(Self.is_null)

    @langkit_property(memoized=True)
    def pred2():
        return Not(Self.parent.is_null)

    # Property calls on null nodes are rejected both in public property
    # wrappers, and in generated code for field accesses. The only supported
    # way to get them called with null nodes is through logic predicates.

    v = UserField(T.LogicVar, public=False)

    @langkit_property()
    def create_equation(with_pred1=T.Bool, n=T.FooNode.entity):
        return (
            Bind(Self.v, n)
            & If(
                with_pred1,
                Predicate(T.FooNode.pred1, Self.v),
                Predicate(T.FooNode.pred2, Self.v),
            )
        )

    @langkit_property(public=True)
    def p1(n=T.FooNode.entity):
        return Self.create_equation(True, n).solve

    @langkit_property(public=True)
    def p2(n=T.FooNode.entity):
        return Self.create_equation(False, n).solve


class Example(FooNode):
    pass


build_and_run(lkt_file="expected_concrete_syntax.lkt", gpr_mains=["main.adb"])
print("Done")
