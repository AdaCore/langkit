"""
Regression test: the expansion of nested iterations (.map, .filter, ...) used
to generate abstract expressions with conflicting names for the iteration
variables, leading to invalid generated Ada code (compilation error: one
variable being an entity, another being a bare node, one masking the other, and
thus having an entity whene a bare node was expected).
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Struct, UserField
from langkit.expressions import Self, langkit_property

from utils import build_and_run


class MyStruct(Struct):
    n = UserField(type=T.FooNode)
    e = UserField(type=T.FooNode.entity)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop():
        return Self.entity_array.mapcat(
            lambda elt1: elt1.node_array.filtermap(
                lambda elt2: MyStruct.new(n=elt2, e=elt1),
                lambda elt2: True,
            )
        )

    @langkit_property()
    def entity_array():
        return Self.as_bare_entity.singleton

    @langkit_property()
    def node_array():
        return Self.singleton


class Example(FooNode):
    token_node = True


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
