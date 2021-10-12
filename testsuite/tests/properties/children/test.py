"""
Check that the "children" builtin property behaves properly both in the DSL and
in the public API.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.expressions import Entity, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def node_children():
        return Self.children.map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def given_node_children(n=T.FooNode):
        return n.children.map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def entity_children():
        return Entity.children

    @langkit_property(public=True)
    def given_entity_children(n=T.FooNode.entity):
        return n.children


class Sequence(FooNode):
    items = Field(type=T.Example.list)


class Example(FooNode):
    token_node = True


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py")
print("Done")
