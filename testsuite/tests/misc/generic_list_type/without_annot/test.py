"""
Check that, if the "generic_list_type" is absent, the generic list type is
assigned a name that can be referred to in the declaration of parse fields
(here: FooNodeBaseList).
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field

from utils import build_and_run


class FooNode(ASTNode):
    pass


class MixedList(FooNode):
    list_node = Field(type=T.FooNodeBaseList)


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
