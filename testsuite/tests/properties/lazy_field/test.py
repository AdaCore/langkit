"""
Check that code emission for lazy fields is correct.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, MetadataField, Struct, abstract, env_metadata
from langkit.expressions import Entity, Self, langkit_property, lazy_field

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    flag = MetadataField(type=T.Bool, use_in_eq=True)


class FooNode(ASTNode):
    pass


@abstract
class AbstractNode(FooNode):

    # Regression test: we used to generate bad Ada code for the initializer of
    # a class for which one of the ancestor has a lazy field.

    @lazy_field(public=True)
    def lazy_in_abstract():
        return 42


class Example(AbstractNode):
    token_node = True

    # Simple test (lazy field is an integer)
    @lazy_field(public=True, activate_tracing=True)
    def lazy_int():
        return 42

    # More complex testcase (lazy field is a ref-counted resources)
    @lazy_field(public=True, activate_tracing=True)
    def lazy_refcounted():
        return [1, 2]

    # Helpers to check that entity info is propagated when retuning the value
    # of a lazy field.

    @langkit_property(public=True)
    def is_flag_enabled():
        return Entity.info.md.flag

    @langkit_property(public=True)
    def with_flag(flag=T.Bool):
        return T.Example.entity.new(
            node=Self,
            info=T.entity_info.new(
                md=T.Metadata.new(flag=flag),
                rebindings=Entity.info.rebindings,
                from_rebound=Entity.info.from_rebound,
            )
        )

    @lazy_field(public=True, return_type=T.Example)
    def lazy_node():
        return Self


class NullNode(AbstractNode):
    token_node = True

    # Regression test: we used to generate invalid Ada code when two lazy
    # fields in different nodes had the same name.
    @lazy_field(public=True)
    def lazy_int():
        return 21


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
