"""
Check that infinite recursion in memoized properties behaves as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, BoolType
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=BoolType)
    def recurse():
        return Self.recurse


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, ada_main='main.adb')
print('Done')
