"""
Check that Langkit_Support.Generic_API.Introspection.Syntax_Field_Index works
as expected.
"""

from langkit.dsl import ASTNode, AbstractField, Field, NullField, T, abstract

from utils import build_and_run


class FooNode(ASTNode):
    # Field that is null only in some concrete derivations
    fld_1 = AbstractField(type=T.FooNode)

    # Field that is null only in some abstract derivations
    fld_2 = AbstractField(type=T.FooNode)

    # Field that is never null
    fld_3 = AbstractField(type=T.FooNode)


@abstract
class A1(FooNode):
    fld_2 = NullField()
    fld_3 = Field(type=T.FooNode)

    only_on_a1 = AbstractField(type=T.FooNode)


class A1B1(A1):
    fld_1 = Field(type=T.FooNode)
    only_on_a1 = NullField()


class A1B2(A1):
    fld_1 = NullField()
    only_on_a1 = NullField()


@abstract
class A2(FooNode):
    fld_1 = Field(type=T.FooNode)


class A2B1(A2):
    fld_2 = Field(type=T.FooNode)
    fld_3 = Field(type=T.FooNode)


class A2B2(A2):
    # Check that the indexes for fld_3 and fld_2 for this node are swapped
    # compared to A2B1.
    fld_3 = Field(type=T.FooNode)
    fld_2 = Field(type=T.FooNode)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    ada_main=["main.adb"],
    types_from_lkt=True,
)
print("Done")
