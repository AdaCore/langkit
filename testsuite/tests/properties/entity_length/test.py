"""
Check that map expressions on entity types work properly.
"""

from langkit.dsl import ASTNode, Field
from langkit.expressions import Entity, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Names(FooNode):
    names = Field()

    @langkit_property(public=True)
    def count():
        return Entity.names.length


class Name(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
