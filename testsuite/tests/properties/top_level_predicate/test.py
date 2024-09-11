"""
Test that we create Predicate on properties of the root AST node.
"""

from langkit.dsl import ASTNode, LogicVar, UserField
from langkit.expressions import Predicate, Self, Var, ignore, langkit_property

from utils import build


class FooNode(ASTNode):
    @langkit_property()
    def sophisticated_predicate():
        return True


class Example(FooNode):
    var1 = UserField(LogicVar, public=False)

    @langkit_property(public=True)
    def prop():
        ignore(Var(Predicate(FooNode.sophisticated_predicate, Self.var1)))
        return Self.as_bare_entity


build(lkt_file='expected_concrete_syntax.lkt', types_from_lkt=True)
print('Compilation was successful')
