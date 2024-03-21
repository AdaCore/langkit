"""
Check that the ".empty" builtin method works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Bool)
    def check_list(n=T.Example.list):
        return n.empty

    @langkit_property(public=True, return_type=T.Bool)
    def check_array(ints=T.Int.array):
        return ints.empty


class Sequence(FooNode):
    items = Field(type=T.Example.list)


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
