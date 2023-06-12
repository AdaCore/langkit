"""
Check that the "exception_name" C API function works as expected.
"""

from langkit.dsl import ASTNode

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.c"],
    unparse_script=unparse_all_script,
    types_from_lkt=True,
)
print('Done')
