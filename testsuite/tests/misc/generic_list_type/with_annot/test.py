"""
Check that, if the "generic_list_type" is present, it gives generic list type
its name (here: FooList).
"""

import sys

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Annotations, Field

from utils import build_and_run


class FooNode(ASTNode):
    annotations = Annotations(generic_list_type="FooList")


class MixedList(FooNode):
    list_node = Field(type=T.FooList)


class Example(FooNode):
    token_node = True


class NullNode(FooNode):
    token_node = True


class SingleList(FooNode):
    list_node = Field(type=T.Example.list)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=False,
)
print("Done")
