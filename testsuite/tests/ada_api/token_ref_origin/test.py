"""
Check that the Origin_Filename/Origin_Charset functions for Token_Reference
work as expected.
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb',
              types_from_lkt=True)
print('Done')
