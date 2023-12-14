"""
Check that expressions processing null lists as collections work they would on
an empty list. They must not trigger property errors.
"""

from langkit.dsl import ASTNode
from langkit.expressions import No, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def test_contains():
        return No(Example.list).contains(Self)

    @langkit_property(public=True)
    def test_filter():
        return (
            No(Example.list)
            .filter(lambda n: n.is_null)
            .map(lambda n: n.as_bare_entity)
        )

    @langkit_property(public=True)
    def test_filtermap():
        return No(Example.list).filtermap(
            lambda n: n.as_bare_entity,
            lambda n: n.is_null,
        )

    @langkit_property(public=True)
    def test_map():
        return No(Example.list).map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def test_mapcat():
        return No(Example.list).mapcat(lambda n: n.as_bare_entity.singleton)

    @langkit_property(public=True)
    def test_takewhile():
        return (
            No(Example.list)
            .take_while(lambda n: n.is_null)
            .map(lambda n: n.as_bare_entity)
        )

    @langkit_property(public=True)
    def test_as_array():
        return No(Example.list).as_array.map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def test_all():
        return No(Example.list).all(lambda n: n.is_null)

    @langkit_property(public=True)
    def test_any():
        return No(Example.list).any(lambda n: n.is_null)

    @langkit_property(public=True)
    def test_at():
        return No(Example.list).at(0).as_bare_entity

    @langkit_property(public=True)
    def test_at_or_raise():
        return No(Example.list).at_or_raise(0).as_bare_entity

    @langkit_property(public=True)
    def test_length():
        return No(Example.list).length


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
