"""Test that public Ada APIs are protected against invalid null arguments."""

from langkit.dsl import ASTNode, Field
from langkit.expressions import langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop():
        return True


class Identifier(FooNode):
    token_node = True


class Decl(FooNode):
    name = Field(type=Identifier)
    value = Field(type=Identifier)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
