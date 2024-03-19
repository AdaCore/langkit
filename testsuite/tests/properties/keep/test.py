"""
Check that the ".keep" property construct works as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field
from langkit.expressions import Entity, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


class Number(FooNode):
    token_node = True


class Sequence(FooNode):
    items = Field(type=T.FooNode.list)

    @langkit_property(public=True, return_type=T.Identifier.entity.array)
    def keep_list():
        return Entity.items.keep(T.Identifier)

    @langkit_property(public=True, return_type=T.Identifier.entity.array)
    def keep_array(items=T.FooNode.entity.array):
        return items.keep(T.Identifier)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    py_script="main.py",
    types_from_lkt=True,
)
print("Done")
