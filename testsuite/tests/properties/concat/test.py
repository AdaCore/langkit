"""
Check that array concatenation works as expected.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.Int.array)
    def int_concat(a=T.Int.array, b=T.Int.array):
        return a.concat(b)

    @langkit_property(public=True, return_type=T.BigInt.array)
    def big_int_concat(a=T.BigInt.array, b=T.BigInt.array):
        return a.concat(b)


class Example(FooNode):
    token_node = True


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
