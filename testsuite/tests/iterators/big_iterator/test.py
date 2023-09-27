"""
Test iterating over a huge number of elements.

This shows that an iterator should be used in place of an array when one wants
to return a huge number of elements.

Indeed, calling ``get_as_iterator`` from public APIs with a huge array will
return a valid iterator on which user can iterate. On the other hand, calling
``get_as_array`` with the same input array will cause a crash.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import build_and_run, unparse_all_script


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.FooNode.entity.array)
    def get_as_array():
        return Self.children.map(lambda n: n.as_bare_entity)

    @langkit_property(public=True, return_type=T.FooNode.entity.iterator)
    def get_as_iterator():
        return Self.get_as_array.to_iterator


class Example(FooNode):
    pass


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    unparse_script=unparse_all_script,
)
print("Done")
