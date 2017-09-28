"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType
from langkit.expressions import Cond, No, Self, langkit_property
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property()
    def identity(b=BoolType):
        return b

    @langkit_property(public=True)
    def cond0():
        return Cond(1)

    @langkit_property(public=True)
    def cond1(b=BoolType):
        return Cond(Self.identity(b), 1,
                    2)

    @langkit_property(public=True)
    def cond2(b1=BoolType, b2=BoolType):
        return Cond(Self.identity(b1), 1,
                    Self.identity(b2), 2,
                    3)

    @langkit_property(public=True)
    def cond3(b1=BoolType, b2=BoolType, b3=BoolType):
        return Cond(Self.identity(b1), 1,
                    Self.identity(b2), 2,
                    Self.identity(b3), 3,
                    3)

    @langkit_property(public=True)
    def cond_node(b=BoolType):
        return Cond(Self.identity(b), Self,
                    No(FooNode)).as_bare_entity


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Example)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
