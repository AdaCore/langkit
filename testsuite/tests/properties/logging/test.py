"""
Check that instrumentation properties logging is working property.
"""

from langkit.dsl import (ASTNode, Bool, Field, MetadataField, Struct, T,
                         env_metadata)
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import (New, RefCategories, Self, Var, ignore,
                                 langkit_property)

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    b = MetadataField(type=Bool, use_in_eq=True)


class FooNode(ASTNode):
    pass


class HasPlus(FooNode):
    enum_node = True
    qualifier = True


class Name(FooNode):
    token_node = True


class Decl(FooNode):
    has_plus = Field()
    name = Field()
    items = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self,
                      metadata=New(Metadata, b=Self.has_plus.as_bool))
    )

    @langkit_property(activate_tracing=True)
    def id_ref_cat(cats=T.RefCategories):
        return cats

    @langkit_property(public=True, return_type=T.Ref.entity.array,
                      activate_tracing=True)
    def entity_items():
        cats = Var(Self.id_ref_cat(RefCategories(default=True)))
        ignore(cats)
        return Self.as_entity.items.map(lambda i: i)


class Ref(FooNode):
    name = Field()


build_and_run(lkt_file="expected_concrete_syntax.lkt", gpr_mains=["main.adb"])
print("Done")
