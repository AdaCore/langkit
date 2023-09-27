"""
Regression test for a code generation bug: we used to have compilation errors
in the generated library if dealing with an "index" precomputed symbol.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Symbol)
    def prop():
        return "index"


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
