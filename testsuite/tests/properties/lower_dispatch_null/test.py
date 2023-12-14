"""
Test that property dispatchers properly fail (i.e. raising a Property_Error
instead of dereferencing a null access) when their Self argument is null.
"""

from langkit.dsl import ASTNode, T, UserField, abstract
from langkit.expressions import (AbstractKind, No, Predicate, Self,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    v = UserField(type=T.LogicVar, public=False)


@abstract
class SomeNode(FooNode):

    @langkit_property(kind=AbstractKind.abstract, return_type=T.Bool)
    def test_prop():
        pass


class Example(SomeNode):

    @langkit_property()
    def test_prop():
        return True

    @langkit_property(public=True)
    def solve():
        return (Self.v.domain([No(T.FooNode.entity)]) &
                Predicate(SomeNode.test_prop, Self.v)).solve


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
