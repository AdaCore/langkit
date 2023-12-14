"""
Test that the overloaded "!=" operator on abstract expressions works as
expected.
"""

from langkit.dsl import ASTNode, Bool, Int, T
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True, return_type=Bool)
    def integers_neq(a=Int, b=Int):
        return a != b


class Example(FooNode):
    token_node = True

    @langkit_property(public=True, return_type=Bool)
    def not_eq(other=T.Example):
        return Self != other


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
