"""
Check that map expressions on entity types work properly.
"""

from langkit.dsl import (ASTNode, Bool, Field, Struct, T, UserField,
                         env_metadata)
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import Entity, New, Self, langkit_property

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    b = UserField(type=Bool)


class FooNode(ASTNode):

    @langkit_property(public=True)
    def b_set():
        return Entity.info.md.b


class Name(FooNode):
    token_node = True


class HasPlus(FooNode):
    enum_node = True
    qualifier = True


class Decl(FooNode):
    has_plus = Field()
    name = Field()
    items = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self,
                      metadata=New(Metadata, b=Self.has_plus.as_bool))
    )

    @langkit_property(public=True, return_type=T.Ref.entity.array)
    def entity_items():
        return Self.as_entity.items.map(lambda i: i)


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True, return_type=Decl.entity)
    def decl():
        return Self.children_env.get(Self.name).at(0).cast_or_raise(Decl)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
