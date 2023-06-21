"""
Common module to the "Node Identity" Java bindings test, defining the Langkit
language to use.
"""

from langkit.dsl import ASTNode, MetadataField, Struct, T, env_metadata
from langkit.expressions import Entity, Self, langkit_property


class FooNode(ASTNode):
    @langkit_property(public=True)
    def with_md(md1=T.Bool, md2=T.Bool):
        return FooNode.entity.new(node=Self, info=T.entity_info.new(
            rebindings=Entity.info.rebindings,
            md=T.Metadata.new(md1=md1, md2=md2),
            from_rebound=Entity.info.from_rebound
        ))


@env_metadata
class Metadata(Struct):
    md1 = MetadataField(type=T.Bool, use_in_eq=True)
    md2 = MetadataField(type=T.Bool, use_in_eq=False)


class Example(FooNode):
    pass
