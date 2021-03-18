"""
Test that stack overflow migitations work as expected in parsing code.
"""

from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True


class Paren(FooNode):
    contents = Field(type=FooNode)


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py",
              types_from_lkt=True)
print("Done")
