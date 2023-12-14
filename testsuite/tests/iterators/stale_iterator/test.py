"""
Test the safety mechanism of iterators: given an iterator created from a
context version X, iterating through it when the current context version is
any Y > X should raise an exception.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import (
    ArrayLiteral, Entity, langkit_property
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Example.entity.iterator)
    def entities_iterator():
        return ArrayLiteral(
            [Entity, Entity, Entity],
            element_type=Example.entity
        ).to_iterator


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
