"""
Common module to the "Node Identity" Java bindings test, defining the Langkit
language to use.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property


class FooNode(ASTNode):

    @langkit_property(
        return_type=T.Bool,
        external=True,
        uses_entity_info=False,
        uses_envs=False,
        public=True
    )
    def trigger_unit_requested(name=T.Symbol, found=T.Bool, error=T.Bool):
        pass


class Example(FooNode):
    token_node = True
