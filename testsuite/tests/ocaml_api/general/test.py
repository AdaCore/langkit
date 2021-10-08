"""
Test that OCaml API is properly working.
"""

from langkit.compile_context import LibraryEntity
from langkit.dsl import (
    ASTNode, Bool, Enum, EnumValue, Field, Struct, Symbol, T, UserField,
    has_abstract_list
)
from langkit.expressions import (
    ArrayLiteral, CharacterLiteral, Entity, If, New, No, Property,
    langkit_property
)

from utils import build_and_run


class Color(Enum):
    Red = EnumValue()
    Green = EnumValue()
    Blue = EnumValue()


class FooNodeStruct(Struct):
    node = UserField(T.FooNode.entity)


class SomeStruct(Struct):
    examples = UserField(T.Example.entity.array)


@has_abstract_list
class FooNode(ASTNode):

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
    def identity(c=T.Character):
        return c

    @langkit_property(public=True)
    def get_str(s=T.String):
        return s

    @langkit_property(public=True)
    def same_color(c=Color):
        return c

    @langkit_property(public=True)
    def same_color_dflt(c=(Color, Color.Red)):
        return c

    @langkit_property(public=True)
    def int_double(c=T.BigInt):
        return c + c

    @langkit_property(public=True)
    def me(b=Bool):
        return FooNodeStruct.new(node=If(b, Entity, No(FooNode.entity)))

    @langkit_property(public=True)
    def get_node(node_struct=FooNodeStruct):
        return node_struct.node

    @langkit_property(public=True)
    def iter_int():
        return ArrayLiteral([1, 2, 3]).to_iterator


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


build_and_run(lkt_file='expected_concrete_syntax.lkt', ocaml_main='main',
              symbol_canonicalizer=LibraryEntity('Pkg', 'Canonicalize'))
print('Done')
