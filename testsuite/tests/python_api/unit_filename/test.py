"""
Test getting the filename corresponding to an analysis unit.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))

build_and_run(foo_grammar, 'main.py')

print('Done')
