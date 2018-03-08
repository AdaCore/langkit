"""
Test that sloc-based token lookup works properly.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.parsers import Grammar, List

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(main_rule=List(Example('example')))
build_and_run(g, ada_main='main.adb')
print('Done')
