"""
Test that a rule with multiple cut parsers works correctly.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Def(FooNode):
    id1 = Field()
    id2 = Field()
    id3 = Field()


class Dot(FooNode):
    id1 = Field()
    id2 = Field()
    id3 = Field()


class Comma(FooNode):
    id1 = Field()
    id2 = Field()
    id3 = Field()


class Id(FooNode):
    token_node = True


class Var(FooNode):
    id = Field()
    ids = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py')
print('Done')
