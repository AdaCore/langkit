"""
Perform general checkings on the C API.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Int.array)
    def int_array_id(ints=T.Int.array):
        return ints.map(lambda i: i + 1)


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.c"],
    types_from_lkt=True,
)

print("Done")
