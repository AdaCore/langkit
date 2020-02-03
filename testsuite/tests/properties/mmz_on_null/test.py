"""
Test that calling a memoized property on a null node triggers the expected
error.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, memoized=True)
    def prop():
        return True


class Example(FooNode):
    pass


g = Grammar('main_rule')
g.add_rules(main_rule=Example('example'))
build_and_run(g, ada_main='main.adb')
print('Done')
