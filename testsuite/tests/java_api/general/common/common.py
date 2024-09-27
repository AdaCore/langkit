"""
Common module to the "General" java bindings test, defining the Langkit
language to use.
"""

from langkit.dsl import (
    ASTNode, Bool, Enum, EnumValue, Field, MetadataField, Struct, Symbol,
    T, UserField, env_metadata, has_abstract_list,
)
from langkit.expressions import (
    ArrayLiteral, CharacterLiteral, Entity, If, New, No, Property,
    PropertyError, Self, langkit_property
)


class Color(Enum):
    red = EnumValue()
    green = EnumValue()
    blue = EnumValue()


class FooNodeStruct(Struct):
    node = UserField(T.FooNode.entity)


class BareNodeStruct(Struct):
    a_boolean = UserField(Bool)
    bare_node = UserField(T.FooNode)
    a_char = UserField(T.Character)


class SomeStruct(Struct):
    examples = UserField(T.Example.entity.array)


@env_metadata
class Metadata(Struct):
    eq_used = MetadataField(type=T.Bool, use_in_eq=True)
    eq_ignored = MetadataField(type=T.Bool, use_in_eq=False)


@has_abstract_list
class FooNode(ASTNode):

    @langkit_property(public=True)
    def with_md(eq_used=T.Bool, eq_ignored=T.Bool):
        return FooNode.entity.new(node=Self, info=T.entity_info.new(
            rebindings=Entity.info.rebindings,
            md=T.Metadata.new(eq_used=eq_used, eq_ignored=eq_ignored),
            from_rebound=Entity.info.from_rebound
        ))

    @langkit_property(public=True)
    def count(seq=T.Example.entity.array):
        return seq.length

    @langkit_property(public=True)
    def get_a(c=(T.Character, CharacterLiteral('a'))):
        return c

    @langkit_property(public=True)
    def get_eacute(c=(T.Character, CharacterLiteral(u'\xe9'))):
        return c

    @langkit_property(public=True)
    def get_int_array():
        return [1, 2, 3]

    @langkit_property(public=True)
    def array_len(a=T.Int.array):
        return a.length

    @langkit_property(public=True, return_type=T.FooNode.entity.array)
    def array_prop_error():
        return PropertyError(T.FooNode.entity.array, "this is an eror")

    @langkit_property(public=True)
    def identity(c=T.Character):
        return c

    @langkit_property(public=True)
    def get_str(s=T.String):
        return s

    @langkit_property(public=True)
    def same_color(c=Color):
        return c

    @langkit_property(public=True)
    def same_color_dflt(c=(Color, Color.red)):
        return c

    @langkit_property(public=True)
    def int_double(c=T.BigInt):
        return c + c

    @langkit_property(public=True)
    def me(b=Bool):
        return FooNodeStruct.new(node=If(b, Entity, No(FooNode.entity)))

    @langkit_property(public=True)
    def my_node():
        return BareNodeStruct.new(
            a_boolean=True,
            bare_node=Entity.node,
            a_char=CharacterLiteral('a')
        )

    @langkit_property(public=True)
    def get_node(node_struct=FooNodeStruct):
        return node_struct.node

    @langkit_property(public=True)
    def get_char(s=BareNodeStruct):
        return s.a_char

    @langkit_property(public=True)
    def iter_int():
        return ArrayLiteral([1, 2, 3]).to_iterator

    @langkit_property(
        return_type=T.Bool,
        external=True,
        uses_entity_info=False,
        uses_envs=False,
        public=True
    )
    def trigger_unit_requested(name=T.Symbol, found=T.Bool, error=T.Bool):
        pass


class Sequence(FooNode.list):
    all_items = Property(Entity.map(lambda i: i), public=True)
    example_items = Property(Entity.filtermap(
        lambda i: i.cast_or_raise(T.Example),
        lambda i: i.is_a(T.Example)
    ), public=True)


class Example(FooNode):

    @langkit_property(public=True)
    def singleton():
        return New(SomeStruct, examples=[Entity])


class Null(FooNode):
    pass


class Var(FooNode):
    arg = Field(type=Sequence)


class Ident(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=Symbol)
    def sym(sym=Symbol):
        return sym


class StringLiteral(FooNode):
    pass
