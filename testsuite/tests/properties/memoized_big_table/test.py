"""
Check that big memoization tables are properly free'd. This used to yield a
stack overflow.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, IntegerType
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=IntegerType)
    def compute(i=IntegerType):
        return i + 1


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(foo_grammar, ada_main='main.adb')
print('Done')
