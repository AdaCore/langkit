"""
Check that the ".logic_all"/".logic_any" builtin methods work as expected.
"""

from langkit.dsl import ASTNode, Field, T, UserField
from langkit.expressions import And, Predicate, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class NameSequence(FooNode):
    names = Field(type=T.Name.list)

    @langkit_property(return_type=T.Equation)
    def domains():
        return Self.names.logic_all(lambda n: n.var.domain([n]))

    @langkit_property(public=True, return_type=T.Bool)
    def any_foo():
        """
        Return whether at least one name is "foo".
        """
        return And(
            Self.domains,
            Self.names.logic_any(lambda n: Predicate(T.Name.predicate, n.var)),
        ).solve

    @langkit_property(public=True, return_type=T.Bool)
    def all_foo():
        """
        Return whether all names are "foo".
        """
        return And(
            Self.domains,
            Self.names.logic_all(lambda n: Predicate(T.Name.predicate, n.var)),
        ).solve


class Name(FooNode):
    token_node = True

    var = UserField(type=T.LogicVar, public=False)

    @langkit_property(return_type=T.Bool)
    def predicate():
        return Self.symbol == "foo"


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
