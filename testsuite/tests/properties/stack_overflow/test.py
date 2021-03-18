"""
Test that stack overflow migitations work as expected in property calls.
"""

from langkit.dsl import ASTNode, Int
from langkit.expressions import If, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(public=True, return_type=Int)
    def recurse(n=Int):
        return If(n <= 1,
                  n,
                  Self.recurse(n - 1))


class Example(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
