"""
Test that true alternatives for booleanized Opt parsers do not yield ghost
nodes.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class HasError(FooNode):
    enum_node = True
    qualifier = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=HasError('error'))
build_and_run(foo_grammar, ada_main='main.adb', generate_unparser=True)
print('Done')
