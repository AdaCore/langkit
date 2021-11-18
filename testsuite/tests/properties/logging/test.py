"""
Check that instrumentation properties logging is working property.
"""

from langkit.dsl import (ASTNode, Bool, Field, Struct, T, UserField,
                         env_metadata)
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import New, Self, langkit_property

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    b = UserField(type=Bool)


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

    @langkit_property(public=True, return_type=T.Ref.entity.array,
                      activate_tracing=True)
    def entity_items():
        return Self.as_entity.items.map(lambda i: i)


class Ref(FooNode):
    name = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb')
print('Done')
