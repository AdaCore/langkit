"""
Check node synthetization and a basic use of it in the Python API.
"""

from langkit.dsl import ASTNode, Field, T, synthetic
from langkit.expressions import If, New, No, Self, langkit_property, lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Literal(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True


@synthetic
class SynthNode(FooNode):
    name = Field(type=Name)
    items = Field(type=Literal.list)


@synthetic
class SynthNode2(FooNode):
    name = Field(type=Name)
    items = Field(type=Literal.list, nullable=True)


class LiteralSequence(FooNode):
    name = Field()
    items = Field()

    # SynthNode.items cannot be null when synthetizing SynthNode, but
    # SynthNode2.items can (nullable). Test both cases for both nodes
    # (with_null arguments below): invalid cases should be rejected at run
    # time.
    #
    # Also make sure that we can synthetize nodes both in memoized properties
    # and in lazy fields.

    @langkit_property(memoized=True)
    def new_node(with_null=T.Bool):
        return New(
            SynthNode,
            name=Self.name,
            items=If(with_null, No(Literal.list), Self.items),
        )

    @langkit_property(public=True)
    def prop(with_null=T.Bool):
        return Self.new_node(with_null).as_bare_entity

    @lazy_field()
    def new_node2_null():
        return New(SynthNode2, name=Self.name, items=No(Literal.list))

    @lazy_field()
    def new_node2():
        return New(SynthNode2, name=Self.name, items=Self.items)

    @langkit_property(public=True)
    def prop2(with_null=T.Bool):
        return If(
            with_null,
            Self.new_node2_null,
            Self.new_node2,
        ).as_bare_entity


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
