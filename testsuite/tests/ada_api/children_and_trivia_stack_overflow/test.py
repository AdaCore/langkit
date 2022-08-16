"""
Check that the Children_And_Trivia function works as expected.

A call to Children_And_Trivia can raises a STORAGE_ERROR (stack overflow) if
the nodes are stored on the stack. This test ensure that no stack overflow is
raised now that Children_And_Trivia stores nodes in dedicated allocated memory
(the current implementation uses an Ada vector to store the nodes).
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              gpr_mains=['main.adb'],
              types_from_lkt=True)
print('Done')
