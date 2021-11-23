"""
This test simply emits a diagnostic using the ``Print_Diagnostic`` procedure
from ``Langkit_Support.Diagnostics.Output``, in particular with a sloc range
ending at column 1, which used to raise a Constraint_Error.
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Foo(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb',
              types_from_lkt=True)
print('Done')
