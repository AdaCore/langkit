"""
Check that the various forms of the "map" DSL construct correctly handle the
presence or absence of index.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field, Struct, UserField
from langkit.expressions import Self, String, langkit_property

from utils import build_and_run


class IndexedNumber(Struct):
    index = UserField(type=T.Int, public=True)
    number = UserField(type=T.NumberNode.entity, public=True)


class FooNode(ASTNode):

    @langkit_property()
    def create(index=T.Int, number=T.NumberNode.entity):
        return IndexedNumber.new(index=index, number=number)


class ListNode(FooNode):
    nb_list = Field(type=T.NumberNode.list)

    @langkit_property()
    def nb_list_entities():
        return Self.nb_list.map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def map_no_idx():
        return Self.nb_list_entities.map(
            lambda n: Self.create(0, n)
        )

    @langkit_property(public=True)
    def map_idx():
        return Self.nb_list_entities.map(
            lambda i, n: Self.create(i, n)
        )

    @langkit_property(public=True)
    def filter_no_idx():
        return Self.nb_list_entities.filter(
            lambda n: n.text == String("2")
        )

    @langkit_property(public=True)
    def filter_idx():
        return Self.nb_list_entities.filter(
            lambda i, n: (n.text == String("2")) | (i == 0)
        )

    @langkit_property(public=True)
    def filtermap_no_idx():
        return Self.nb_list_entities.filtermap(
            lambda n: Self.create(0, n),
            lambda n: n.text == String("2")
        )

    @langkit_property(public=True)
    def filtermap_elt_idx():
        return Self.nb_list_entities.filtermap(
            lambda i, n: Self.create(i, n),
            lambda n: n.text == String("2")
        )

    @langkit_property(public=True)
    def filtermap_filter_idx():
        return Self.nb_list_entities.filtermap(
            lambda n: Self.create(0, n),
            lambda i, n: (n.text == String("2")) | (i == 0)
        )

    @langkit_property(public=True)
    def filtermap_all_idx():
        return Self.nb_list_entities.filtermap(
            lambda i, n: Self.create(i, n),
            lambda i, n: (n.text == String("2")) | (i == 0)
        )


class NumberNode(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
