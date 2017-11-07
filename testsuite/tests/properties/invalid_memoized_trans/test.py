"""
Check that memoization for properties that deal with side effects is properly
rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, UserField, T
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar

from os import path
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(path.abspath(__file__))


class FooNode(ASTNode):
    var = UserField(type=T.LogicVarType, public=False)


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
    # ... but using unsafe_memoization, all should be fine!
    #

    # First check that this attribute works on a property that cannot be
    # memoized because it contains an "immediately" unsafe construct.
    @langkit_property(public=True, memoized=True, unsafe_memoization=True)
    def get_var3():
        return Self.var.get_value

    # Then, on one that cannot be memoized because of transitivity
    @langkit_property(public=True, memoized=True, unsafe_memoization=True)
    def get_var2_public_wrapper2():
        return Self.get_var2


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example'),
)
emit_and_print_errors(grammar)
print('Done')
