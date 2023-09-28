"""
Test that Prediate works well with default argument values.
"""

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import (
    And, DynamicVariable, Predicate, Self, Var, langkit_property
)

from utils import build_and_run


my_var = DynamicVariable("my_var", T.Int)


class FooNode(ASTNode):
    pass


class Example(FooNode):

    var1 = UserField(T.LogicVar, public=False)
    var2 = UserField(T.LogicVar, public=False)

    # Simple case: the predicate only has two arguments with default values

    @langkit_property()
    def predicate1(
        n=T.FooNode.entity,
        flag1=(T.Bool, False),
        flag2=(T.Bool, True)
    ):
        return And(flag1, n.is_null, flag2)

    @langkit_property(public=True)
    def prop1():
        eq = Var(Predicate(T.Example.predicate1, Self.var1, Self.var2))
        return eq.solve

    # More complex case: the predicate has one argument with a default value,
    # but also takes a dynamic variable.

    @langkit_property(dynamic_vars=[my_var])
    def predicate2(
        n=T.FooNode.entity,
        flag=(T.Bool, False)
    ):
        return And(n.is_null, my_var == 0, flag)

    @langkit_property(public=True)
    def prop2():
        eq = Var(
            my_var.bind(
                1, Predicate(T.Example.predicate2, Self.var1, Self.var2)
            )
        )
        return eq.solve

    # Variant of the complex case: the predicate takes both one argument with a
    # default value and a dynamic variable with a default value.

    @langkit_property(dynamic_vars=[(my_var, 0)])
    def predicate3(
        n=T.FooNode.entity,
        flag=(T.Bool, False)
    ):
        return And(n.is_null, my_var == 0, flag)

    @langkit_property(public=True)
    def prop3():
        eq = Var(
            my_var.bind(
                1, Predicate(T.Example.predicate3, Self.var1, Self.var2)
            )
        )
        return eq.solve


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
