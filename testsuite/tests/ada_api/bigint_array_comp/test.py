"""
Check the availability of the "=" operator on arrays of big integers.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def prop(a=T.BigInt.array):
        return a


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'],
              types_from_lkt=True)
print('Done')
