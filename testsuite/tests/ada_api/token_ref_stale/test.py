"""
Check that use of a ``Token_Reference`` value that refers to a stale token is
properly rejected.
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
