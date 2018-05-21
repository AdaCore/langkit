"""
Test the behavior of the .is_a operation on null nodes.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.expressions import No, Self, langkit_property
from langkit.parsers import Grammar, List

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def null_node():
        return No(T.FooNode)

    @langkit_property(public=True)
    def is_null_a_def():
        return Self.null_node.is_a(T.Def)


class Def(FooNode):
    example = Field()


class Example(FooNode):
    token_node = True


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=List(Def('def', Example('example'))))
build_and_run(foo_grammar, 'main.py')
print('Done')
