"""
Test that comparing nodes works as expected in the DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def before(n=T.FooNode):
        return Self < n

    @langkit_property(public=True)
    def before_or_equal(n=T.FooNode):
        return Self <= n


class Example(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=List(Example('example')))
build_and_run(foo_grammar, 'main.py')
print('Done')
