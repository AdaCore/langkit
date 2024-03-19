"""
Check that escapes in lexer patterns are correctly handled.
"""

from langkit.dsl import ASTNode

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    unparse_script=unparse_all_script,
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
