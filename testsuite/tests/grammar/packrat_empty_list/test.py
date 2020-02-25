"""
Test that nested trailing empty lists are correctly handled in node creation.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Def(FooNode):
    name = Field()
    defs = Field()
    values = Field()


class Values(FooNode):
    items = Field()


class Number(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
