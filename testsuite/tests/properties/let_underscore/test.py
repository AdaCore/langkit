from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Let, No, Property, Self
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    prop = Property(Let(lambda _=Self.parent: No(FooNode.entity)), public=True)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=BarNode('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
