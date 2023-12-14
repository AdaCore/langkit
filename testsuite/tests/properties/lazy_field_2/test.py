"""
Check that entity info is propagated when retuning the value of a lazy field.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, MetadataField, Struct, env_metadata, synthetic
from langkit.expressions import Entity, Self, langkit_property, lazy_field

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    flag = MetadataField(type=T.Bool, use_in_eq=True)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def is_flag_enabled():
        return Entity.info.md.flag

    @langkit_property(public=True)
    def with_flag(flag=T.Bool):
        return T.entity.new(
            node=Self,
            info=T.entity_info.new(
                md=T.Metadata.new(flag=flag),
                rebindings=Entity.info.rebindings,
                from_rebound=Entity.info.from_rebound,
            )
        )


@synthetic
class SynthNode(FooNode):
    pass


class Example(FooNode):

    @lazy_field(public=True, return_type=T.SynthNode)
    def my_field():
        return T.SynthNode.new()


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
