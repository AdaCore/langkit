"""
Check that infinite recursion in memoized properties behaves as expected.
"""

from langkit.dsl import ASTNode, Bool
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=Bool)
    def recurse():
        return Self.recurse


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
