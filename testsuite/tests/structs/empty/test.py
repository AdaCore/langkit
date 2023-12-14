"""
Check that the generated code for empty structs works as expected.
"""

from langkit.dsl import ASTNode, Struct, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class MyStruct(Struct):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(public=True)
    def identity(s=T.MyStruct):
        return s


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("")
print("Done")
