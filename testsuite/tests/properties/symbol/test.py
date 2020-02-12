"""
Test that ".symbol" raises a property error on null nodes.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop(e=T.Example):
        return e.symbol


class Example(FooNode):
    token_node = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
