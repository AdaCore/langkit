"""
Test that sloc-based token lookup works properly.
"""

from langkit.dsl import ASTNode

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
    unparse_script=unparse_all_script,
    types_from_lkt=True,
)
print("Done")
