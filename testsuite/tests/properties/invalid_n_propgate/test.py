"""
Check that invalid ``NPropagate`` expressions are properly rejected.
"""

from langkit.dsl import ASTNode, LogicVar, T, UserField
from langkit.expressions import (
    Entity, NPropagate, Self, ignore, langkit_property
)

from utils import emit_and_print_errors


def run(name, prop, vars_fn):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print("== {} ==".format(name))

    class FooNode(ASTNode):
        ref_var = UserField(LogicVar, public=False)
        type_var = UserField(LogicVar, public=False)

    class BarNode(FooNode):

        @langkit_property(warn_on_unused=False)
        def main_prop():
            return NPropagate(Self.ref_var, prop(T), *vars_fn(Self))

    class BazNode(FooNode):

        # Make the properties accept a variying number of entities. Use
        # multiple entity derived types, as they should all be accepted. Also
        # add a default argument somewhere, which should not matter when
        # matching the number of logic variables.

        @langkit_property(warn_on_unused=False)
        def prop1():
            return Entity

        @langkit_property(warn_on_unused=False)
        def prop2(e1=T.FooNode.entity, b=(T.Bool, True)):
            ignore(e1)
            ignore(b)
            return Entity

        @langkit_property(warn_on_unused=False)
        def prop3(e1=T.BazNode.entity, e2=T.BarNode.entity):
            ignore(e1)
            ignore(e2)
            return Entity

    emit_and_print_errors(lkt_file="foo.lkt")
    print("")


# Check that the matching between the number of "argument" logic variables and
# the arity of the property works as expected.
run("Valid N=1",
    lambda T: T.BazNode.prop1, lambda Self: (Self.type_var,))
run("Valid N=2",
    lambda T: T.BazNode.prop2, lambda Self: (Self.type_var, Self.type_var))
run("Valid N=3",
    lambda T: T.BazNode.prop3,
    lambda Self: (Self.type_var, Self.type_var, Self.type_var))

run("Bad arity (N=1, got 0)",
    lambda T: T.BazNode.prop1, lambda Self: ())
run("Bad arity (N=1, got 2)",
    lambda T: T.BazNode.prop1, lambda Self: (Self.type_var, Self.type_var))
run("Bad arity (N=2, got 1)",
    lambda T: T.BazNode.prop2, lambda Self: (Self.type_var, ))

print('Done')
