"""
Test that the "Text" function in the Ada API raises a Property_Error when
called on a null node.
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
