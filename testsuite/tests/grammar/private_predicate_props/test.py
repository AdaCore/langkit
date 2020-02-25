"""
Check that having private properties used as predicates in the grammar
generates valid Ada code.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Not, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True

    @langkit_property()
    def is_not_class_id():
        return Not(Self.symbol == 'class')


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
