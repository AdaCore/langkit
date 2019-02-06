"""
Test that getting a unit using different filenames for the same file return the
same unit (i.e. that the filename is canonicalized).
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, abstract
from langkit.expressions import Property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class EnumNode(FooNode):
    prop = Property(True, public=True)


class HasExample(EnumNode):
    enum_node = True
    qualifier = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=HasExample('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
