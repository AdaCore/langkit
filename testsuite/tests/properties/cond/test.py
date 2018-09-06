"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool
from langkit.expressions import Cond, No, Self, langkit_property
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property()
    def identity(b=Bool):
        return b

    @langkit_property(public=True)
    def cond0():
        return Cond(1)

    @langkit_property(public=True)
    def cond1(b=Bool):
        return Cond(Self.identity(b), 1,
                    2)

    @langkit_property(public=True)
    def cond2(b1=Bool, b2=Bool):
        return Cond(Self.identity(b1), 1,
                    Self.identity(b2), 2,
                    3)

    @langkit_property(public=True)
    def cond3(b1=Bool, b2=Bool, b3=Bool):
        return Cond(Self.identity(b1), 1,
                    Self.identity(b2), 2,
                    Self.identity(b3), 3,
                    3)

    @langkit_property(public=True)
    def cond_node(b=Bool):
        return Cond(Self.identity(b), Self,
                    No(FooNode)).as_bare_entity


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
