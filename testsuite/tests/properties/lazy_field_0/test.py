"""
Check that code emission for lazy fields is correct.
"""

from langkit.dsl import ASTNode
from langkit.expressions import lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    # Simple test (lazy field is an integer)
    @lazy_field(public=True, activate_tracing=True)
    def my_field_1():
        return 42

    # More complex testcase (lazy field is a ref-counted resources)
    @lazy_field(public=True, activate_tracing=True)
    def my_field_2():
        return [1, 2]


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
