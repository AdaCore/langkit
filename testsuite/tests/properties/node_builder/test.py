"""
Check that node builders work as expected.
"""

from langkit.dsl import ASTNode, Field, T, UserField, abstract, synthetic
from langkit.expressions import (
    BigIntLiteral,
    CreateCopyNodeBuilder,
    CreateSynthNodeBuilder,
    No,
    Self,
    langkit_property,
    lazy_field,
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True


@synthetic
class SynthUserField(FooNode):
    name = Field(type=Name)
    number = UserField(type=T.BigInt, public=False)

    @langkit_property(public=True)
    def get_number():
        return Self.number


@synthetic
class SynthNonNullable(FooNode):
    items = Field(type=Literal)


@synthetic
class SynthNullable(FooNode):
    items = Field(type=Literal, nullable=True)


@synthetic
class SynthParent(FooNode):
    f = Field(type=FooNode, nullable=True)


@abstract
class SynthBaseChild(FooNode):
    f = Field(type=FooNode, nullable=True)


@synthetic
class SynthChild1(SynthBaseChild):
    pass


@synthetic
class SynthChild2(SynthBaseChild):
    pass


@abstract
class SynthImplicitField1(FooNode):
    pass


@synthetic
class SynthImplicitField2(SynthImplicitField1):
    pass


@synthetic
class SynthImplicitField3(SynthImplicitField1):
    pass


@synthetic
class SynthImplicitFieldParent(FooNode):
    f = Field(type=T.SynthImplicitField1)


class LiteralSequence(FooNode):
    name = Field()
    items = Field()

    # Check that the copy node build behave as expected

    @lazy_field(public=True, return_type=T.LiteralSequence)
    def lf_copy_non_null():
        return CreateCopyNodeBuilder(Self).build()

    @lazy_field(public=True, return_type=T.LiteralSequence)
    def lf_copy_null():
        return CreateCopyNodeBuilder(No(T.LiteralSequence)).build()

    # We need to make sure that user fields (below: SynthUserField.number) are
    # correctly propagated from the builder to the synthetized node.

    @lazy_field(public=True, return_type=T.SynthUserField)
    def lf_user_field():
        return CreateSynthNodeBuilder(
            T.SynthUserField,
            name=CreateCopyNodeBuilder(Self.name),
            number=BigIntLiteral(42),
        ).build()

    # SynthNonNullable.items cannot be null while SynthNullable.items can: we
    # need to check the behavior of both cases for both nodes.

    @lazy_field(public=True, return_type=T.SynthNonNullable)
    def lf_non_nullable_null():
        return CreateSynthNodeBuilder(
            T.SynthNonNullable, items=CreateCopyNodeBuilder(No(Literal))
        ).build()

    @lazy_field(public=True, return_type=T.SynthNonNullable)
    def lf_non_nullable_not_null():
        return CreateSynthNodeBuilder(
            T.SynthNonNullable, items=CreateCopyNodeBuilder(Self.items.at(0))
        ).build()

    @lazy_field(public=True, return_type=T.SynthNullable)
    def lf_nullable_null():
        return CreateSynthNodeBuilder(
            T.SynthNullable, items=CreateCopyNodeBuilder(No(Literal))
        ).build()

    @lazy_field(public=True, return_type=T.SynthNullable)
    def lf_nullable_not_null():
        return CreateSynthNodeBuilder(
            T.SynthNullable, items=CreateCopyNodeBuilder(Self.items.at(0))
        ).build()

    # We need to make sure that synthetized nodes are assigned the right
    # parent: the parent of the builder root is the one given as argument, and
    # the parents of the children follow the builder tree structure.

    @lazy_field(public=True, return_type=T.SynthParent)
    def lf_parent_root():
        return CreateSynthNodeBuilder(
            T.SynthParent,
            f=CreateSynthNodeBuilder(
                T.SynthChild1,
                f=CreateSynthNodeBuilder(
                    T.SynthChild2,
                    f=CreateCopyNodeBuilder(Self.items.at(0)),
                )
            ),
        ).build(Self)

    @lazy_field(public=True, return_type=T.SynthParent)
    def lf_parent_child():
        return CreateSynthNodeBuilder(
            T.SynthParent,
            f=CreateSynthNodeBuilder(
                T.SynthChild1,
                f=CreateCopyNodeBuilder(Self),
            ),
        ).build(Self.items.at(0))

    @lazy_field(public=True, return_type=T.SynthParent)
    def lf_parent_null():
        return CreateSynthNodeBuilder(
            T.SynthParent, f=CreateCopyNodeBuilder(No(T.FooNode))
        ).build()

    # The parent node must not be foreign

    @langkit_property(
        return_type=T.FooNode,
        external=True,
        uses_entity_info=False,
        uses_envs=False,
    )
    def get_foreign_node():
        pass

    @lazy_field(public=True, return_type=T.SynthParent)
    def lf_parent_foreign():
        return CreateSynthNodeBuilder(
            T.SynthParent, f=CreateCopyNodeBuilder(No(T.FooNode))
        ).build(Self.get_foreign_node)

    # Regression test: code generation used to crash when processing the node
    # builder for SynthImplicitFieldParent: its synthetizing node builder
    # requires the node builder for SynthImplicitField1 (i.e. the type of its
    # field), but properties only mention a node builder for
    # SynthImplicitField2.

    @lazy_field(public=True, return_type=T.SynthImplicitFieldParent)
    def lf_implicit_field2():
        return CreateSynthNodeBuilder(
            T.SynthImplicitFieldParent,
            f=CreateSynthNodeBuilder(T.SynthImplicitField2),
        ).build()

    @lazy_field(public=True, return_type=T.SynthImplicitFieldParent)
    def lf_implicit_field3():
        return CreateSynthNodeBuilder(
            T.SynthImplicitFieldParent,
            f=CreateSynthNodeBuilder(T.SynthImplicitField3),
        ).build()


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
