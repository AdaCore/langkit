"""
Check that memoized propoerties that have arguments of array types work as
advertised.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property, ArrayLiteral, If, String
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=T.Int.array)
    def get_array():
        return ArrayLiteral([1, 2])

    @langkit_property(public=True, memoized=True, return_type=T.Int)
    def test_prop(numbers=T.Int.array, c=T.String):
        return If(c == String("one"), numbers.at(0), numbers.at(1))


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, ada_main='main.adb')
print('Done')
