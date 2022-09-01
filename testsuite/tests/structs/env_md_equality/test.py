"""
Test that metadata identity is working as expected.
"""

from langkit.dsl import ASTNode, MetadataField, Struct, T, env_metadata
from langkit.expressions import Entity, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


@env_metadata
class Metadata(Struct):
    md1 = MetadataField(type=T.Bool, use_in_eq=True)
    md2 = MetadataField(type=T.Bool, use_in_eq=False)


class Name(FooNode):
    token_node = True

    @langkit_property(public=True)
    def with_md(md1=T.Bool, md2=T.Bool):
        return FooNode.entity.new(node=Entity.node, info=T.entity_info.new(
            rebindings=Entity.info.rebindings,
            md=T.Metadata.new(md1=md1, md2=md2),
            from_rebound=Entity.info.from_rebound
        ))


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              lkt_semantic_checks=False)
print('')
print('Done')
