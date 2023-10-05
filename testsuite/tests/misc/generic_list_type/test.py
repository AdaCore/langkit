"""
Check that the "generic_list_type" node annotation works as expected.
"""

import sys

import langkit
from langkit.compiled_types import T
from langkit.dsl import ASTNode, Annotations, Field

from utils import build_and_run


def common(label, FooNode):
    class Example(FooNode):
        token_node = True

    class NullNode(FooNode):
        token_node = True

    class SingleList(FooNode):
        list_node = Field(type=T.Example.list)

    print("== {} ==".format(label))
    print()
    sys.stdout.flush()
    build_and_run(
        lkt_file="{}.lkt".format(label),
        gpr_mains=["{}.adb".format(label)],
        unparse_script=None,
        types_from_lkt=False,
    )
    print()
    langkit.reset()


def test_without_annot():
    class FooNode(ASTNode):
        pass

    class MixedList(FooNode):
        list_node = Field(type=T.FooNodeBaseList)

    common("without_annot", FooNode)


def test_with_annot():
    class FooNode(ASTNode):
        annotations = Annotations(generic_list_type="FooList")

    class MixedList(FooNode):
        list_node = Field(type=T.FooList)

    common("with_annot", FooNode)


test_without_annot()
test_with_annot()
print("Done")
