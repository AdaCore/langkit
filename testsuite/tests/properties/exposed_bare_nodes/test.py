"""
Test that the automatic (un)wrapping of bare nodes in the interface of public
properties works as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def compute(n=FooNode):
        return n

grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Example('example'),
)
build_and_run(grammar, 'main.py')

print('')
print('Done')
