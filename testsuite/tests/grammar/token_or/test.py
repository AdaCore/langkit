"""
Check that the case insenvitity feature works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):
    pass


class Assignment(FooNode):
    name = Field(type=T.Identifier)
    value = Field(type=T.Number)


class Number(FooNode):
    token_node = True


class Identifier(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    unparse_script=unparse_all_script,
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
