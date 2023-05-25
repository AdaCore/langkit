"""
Check that looking for the PLE root of a node works as expected.
"""

from langkit.dsl import ASTNode, Annotations, Field
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def get_ple_root():
        return Self.ple_root.as_bare_entity


class Identifier(FooNode):
    token_node = True


class Var(FooNode):
    name = Field(type=Identifier)
    value = Field(type=Identifier)


class Scope(FooNode):
    items = Field(type=Identifier.list)

    annotations = Annotations(ple_unit_root=True)


build_and_run(lkt_file="expected_concrete_syntax.lkt", ada_main="main.adb")
print("Done")
