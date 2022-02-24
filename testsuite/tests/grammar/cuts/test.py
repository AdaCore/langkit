"""
Test that the cut/stop_cut parsers works correctly. For more information, check
the expected_concrete_syntax.lkt file.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Def(FooNode):
    id = Field()


class VarDef(FooNode):
    id = Field()


class Block(FooNode):
    el = Field()


class Id(FooNode):
    token_node = True


build_and_run(lkt_file='foo.lkt',
              py_script='main.py')
print('Done')
