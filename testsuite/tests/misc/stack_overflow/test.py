"""
Test that stack overflow migitations work as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Int
from langkit.expressions import If, Self, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True, return_type=Int)
    def recurse(n=Int):
        return If(n <= 1,
                  n,
                  Self.recurse(n - 1))


class Example(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(main_rule=Example('example'))
build_and_run(g, 'main.py')
print('Done')
