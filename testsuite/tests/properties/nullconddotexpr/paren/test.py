"""
Check that using parens breaks the propagation behavior of null-conditional
operations in Lkt.
"""

from langkit.dsl import ASTNode, Field, T

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Cons(FooNode):
    prefix = Field(type=T.Cons)
    suffix = Field(type=T.Name)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    unparse_script="to:dummy.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
