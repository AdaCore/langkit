"""
Check the behavior of the "null cond dot expr" expression ("._." in the DSL).
"""

from langkit.dsl import ASTNode, Field, T
from langkit.expressions import Self, ignore, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True)
    def field_1(n=T.Cons):
        return n._.prefix

    @langkit_property(public=True)
    def field_2(n=T.Cons):
        return n._.prefix.prefix

    @langkit_property(public=True)
    def field_3(n=T.Cons):
        return n.prefix._.prefix

    @langkit_property(public=True)
    def field_4(n=T.Cons):
        return n._.prefix._.prefix

    @langkit_property(public=True)
    def call_1(n=T.Cons):
        return n._.get_prefix(Self)

    @langkit_property(public=True)
    def call_2(n=T.Cons):
        return n._.get_prefix(Self).get_prefix(Self)

    @langkit_property(public=True)
    def call_3(n=T.Cons):
        return n.get_prefix(Self)._.get_prefix(Self)

    @langkit_property(public=True)
    def call_4(n=T.Cons):
        return n._.get_prefix(Self)._.get_prefix(Self)


class Name(FooNode):
    token_node = True


class Cons(FooNode):
    prefix = Field(type=T.Cons)
    suffix = Field(type=T.Name)

    @langkit_property()
    def get_prefix(n=T.FooNode):
        ignore(n)
        return Self.prefix


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
