"""
Check that memoization for properties that deal with side effects is properly
rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, UserField
from langkit.expressions import Entity, Self, langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    var = UserField(type=T.LogicVar, public=False)


class Example(FooNode):

    # Most simple case: trying to memoize a property that does something with
    # side effect.
    @langkit_property(public=True, memoized=True)
    def get_var():
        return Self.var.get_value

    # This one is just a wrapper, to demonstrate transitivity
    @langkit_property(public=True)
    def get_var_wrapper():
        return Self.get_var

    # This one is not memoized, but is another property with side effects
    @langkit_property(public=True)
    def get_var2():
        return Self.var.get_value

    # This tries to memoize a property that calls one which does directly a
    # side effect (but the latter is not memoized).
    @langkit_property(public=True, memoized=True)
    def get_var2_public_wrapper():
        return Self.get_var2

    # This is memoized and calling two properties that do side effect by
    # transitivity.
    @langkit_property(public=True, memoized=True)
    def top_wrapper():
        return Self.get_var_wrapper == Self.get_var2

    #
    # ... but using call_memoizable, all should be fine!
    #

    # First check that this attribute works on a property that cannot be
    # memoized because it contains an "immediately" unsafe construct.
    @langkit_property(public=True, memoized=True, call_memoizable=True)
    def get_var3():
        return Self.var.get_value

    # Then, on one that cannot be memoized because of transitivity
    @langkit_property(public=True, memoized=True, call_memoizable=True)
    def get_var2_public_wrapper2():
        return Self.get_var2

    # Now, check that explicitely telling Langkit that a property cannot be
    # used transitively in memoization works.
    @langkit_property(
        public=True, external=True, return_type=T.Entity,
        uses_entity_info=True, uses_envs=True,
        call_non_memoizable_because='get_something_unmemoizable is'
                                    ' unmemoizable')
    def get_something_unmemoizable():
        return

    @langkit_property(public=True)
    def get_var4():
        return Entity.get_something_unmemoizable

    @langkit_property(public=True, memoized=True)
    def get_var4_public_wrapper():
        return Entity.get_var4


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
