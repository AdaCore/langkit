"""
Check the specific behavior of several properties (.is_synthetic, .text and
.sloc_range) when given a synthetic node.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, synthetic
from langkit.expressions import New, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    @langkit_property(memoized=True, public=True)
    def get():
        return New(SynthNode)


@synthetic
class SynthNode(FooNode):
    pass


build_and_run(lkt_file='expected_concrete_syntax.lkt',
              py_script='main.py', ada_main='main.adb')
print('Done')
