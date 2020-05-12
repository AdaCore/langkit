"""
Test that Adalog's Early_Binding_Error exception is turned into a
Property_Error exception.
"""

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import Predicate, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    a = UserField(T.LogicVar, public=False)

    @langkit_property()
    def pred():
        return False

    @langkit_property(public=True)
    def do_solving():
        return Predicate(Example.pred, Self.a).solve


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
