"""
Check that the synthetization of a node that involves a lazy field works as
expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, synthetic
from langkit.expressions import Self, langkit_property, lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @lazy_field(return_type=T.SynthNode)
    def new_node():
        return T.SynthNode.new()

    @langkit_property(return_type=T.Int, public=True)
    def prop():
        return Self.new_node.lf


@synthetic
class SynthNode(FooNode):

    @lazy_field(return_type=T.Int)
    def lf():
        return 42


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
