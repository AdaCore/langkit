"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def can_reach(n=T.FooNode.entity, from_node=T.FooNode.entity):
        return n.el.can_reach(from_node.el)


class Example(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=List(Example('example')))
build_and_run(foo_grammar, 'main.py')
print('Done')
