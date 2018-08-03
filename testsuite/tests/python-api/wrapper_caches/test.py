"""
Test that the various wrappers that the Python binding instantiates (contexts,
units, nodes) are re-used whenever we want to wrap unique C values.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.parsers import Grammar, List, Opt

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    examples = Field()


g = Grammar('main_rule')
g.add_rules(main_rule=g.example_list,
            example_list=List(g.example),
            example=Example('example', Opt('(', g.example_list, ')')))
build_and_run(g, 'main.py')
print('Done')
