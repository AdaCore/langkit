"""
Check that memoized properties that raise an exception work as expected.
"""

from langkit.dsl import ASTNode, Bool
from langkit.expressions import (
    If, PreconditionFailure, PropertyError, Self, Var, ignore, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    # Check with a property that has finalizers, because it handles refcounted
    # objects.
    @langkit_property(public=True, memoized=True, return_type=Bool)
    def prop1():
        ignore(Var(Self.singleton))
        return PropertyError(Bool, "Explicit error 1")

    # Also check with one that does not
    @langkit_property(public=True, memoized=True, return_type=Bool)
    def prop2():
        return PropertyError(Bool, "Explicit error 2")

    # Check that non-Property_Error are also properly handled

    @langkit_property(public=True, memoized=True, return_type=Bool)
    def prop3():
        return Self.raise_error(False)

    @langkit_property(return_type=Bool)
    def raise_error(prop_error=Bool):
        return If(
            prop_error,
            PropertyError(Bool, "This is a Property_Error"),
            PreconditionFailure(Bool, "This is a Precondition_Failure"),
        )


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    property_exceptions={"Precondition_Failure"},
    types_from_lkt=True,
)
print("Done")
