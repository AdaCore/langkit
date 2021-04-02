"""
Check that the "parents" builtin property behaves properly both in the DSL and
in the public API.
"""

from langkit.dsl import ASTNode
from langkit.expressions import Entity, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True)
    def node_parents():
        return Self.parents.map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def node_parents_without_self():
        return Self.parents(with_self=False).map(lambda n: n.as_bare_entity)

    @langkit_property(public=True)
    def entity_parents():
        return Entity.parents

    @langkit_property(public=True)
    def entity_parents_without_self():
        return Entity.parents(with_self=False)


class Example(FooNode):
    token_node = True


build_and_run(lkt_file="expected_concrete_syntax.lkt", py_script="main.py")
print("Done")
