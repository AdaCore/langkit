"""
Check that the list parser "extra" separator specifier works as expected.
"""

from langkit.dsl import ASTNode

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)

print("Done")
