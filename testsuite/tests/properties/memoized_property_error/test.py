"""
Check that memoized properties that raise a PropertyError work as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, BoolType
from langkit.expressions import (PropertyError, Self, Var, ignore,
                                 langkit_property)
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    # Check with a property that has finalizers, because it handles refcounted
    # objects.
    @langkit_property(public=True, memoized=True, return_type=BoolType)
    def prop1():
        ignore(Var(Self.singleton))
        return PropertyError(BoolType, 'Explicit error 1')

    # Also check with one that does not
    @langkit_property(public=True, memoized=True, return_type=BoolType)
    def prop2():
        return PropertyError(BoolType, 'Explicit error 2')


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, ada_main='main.adb')
print('Done')
