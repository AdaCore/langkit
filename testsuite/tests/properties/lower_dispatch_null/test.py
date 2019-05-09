"""
Test that property dispatchers properly fail (i.e. raising a Property_Error
instead of dereferencing a null access) when their Self argument is null.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, UserField, abstract
from langkit.expressions import (AbstractKind, No, Predicate, Self,
                                 langkit_property)
from langkit.parsers import Grammar

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


grammar = Grammar('main_rule')
grammar.add_rules(main_rule=Example('example'))
build_and_run(grammar, 'main.py')
print('Done')
