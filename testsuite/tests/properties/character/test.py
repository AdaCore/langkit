"""
Test that the Character type works as expected in generated APIs.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import (ArrayLiteral, CharacterLiteral,
                                 langkit_property)
from langkit.parsers import Grammar

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def get_a(c=(T.Character, CharacterLiteral('a'))):
        return c

    @langkit_property(public=True)
    def get_eacute(c=(T.Character, CharacterLiteral(u'\xe9'))):
        return c

    @langkit_property(public=True)
    def identity(c=T.Character):
        return c

    @langkit_property(public=True)
    def double(c=T.Character):
        return ArrayLiteral([c, c], T.Character)

    @langkit_property(public=True)
    def text_identity(s=T.String):
        return s


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(main_rule=Example('example'))
build_and_run(foo_grammar, 'main.py')
print('Done')
