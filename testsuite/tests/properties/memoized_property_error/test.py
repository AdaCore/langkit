"""
Check that memoized properties that raise a PropertyError work as expected.
"""

from langkit.dsl import ASTNode, Bool
from langkit.expressions import (PropertyError, Self, Var, ignore,
                                 langkit_property)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    # Check with a property that has finalizers, because it handles refcounted
    # objects.
    @langkit_property(public=True, memoized=True, return_type=Bool)
    def prop1():
        ignore(Var(Self.singleton))
        return PropertyError(Bool, 'Explicit error 1')

    # Also check with one that does not
    @langkit_property(public=True, memoized=True, return_type=Bool)
    def prop2():
        return PropertyError(Bool, 'Explicit error 2')


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb',
              lkt_semantic_checks=True)
print('Done')
