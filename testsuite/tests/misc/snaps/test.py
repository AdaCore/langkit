"""
Check that the "snaps" node annotation works as expected.
"""

from langkit.dsl import ASTNode, Annotations, Field, has_abstract_list

from utils import build_and_run


class FooNode(ASTNode):
    pass


@has_abstract_list
class Example(FooNode):
    token_node = True


@has_abstract_list
class NullNode(FooNode):
    token_node = True


class WithSnapList(Example.list):
    annotations = Annotations(snaps=True)


class WithoutSnapList(NullNode.list):
    pass


class ListWrapper(FooNode):
    item = Field()


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
