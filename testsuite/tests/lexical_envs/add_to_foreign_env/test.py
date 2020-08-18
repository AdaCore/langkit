"""
Check that add_to_env actions that try to add an entry to a foreign
environment without the unsafe=True flag raise an error.
"""

from langkit.dsl import (ASTNode, Field, Struct, T, UserField, abstract,
                         env_metadata)
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import AbstractKind, Self, langkit_property

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    node = UserField(T.FooNode)


class FooNode(ASTNode):
    pass


class Scope(FooNode):
    name = Field(type=T.SimpleId)
    content = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self),
        add_env()
    )


@abstract
class Id(FooNode):
    @langkit_property(return_type=T.FooNode, kind=AbstractKind.abstract)
    def resolve(base_env=T.LexicalEnv):
        pass


class SimpleId(Id):
    token_node = True

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return base_env.get_first(Self.symbol).node


class ScopedId(Id):
    scope = Field(type=T.Id)
    name = Field(type=T.SimpleId)

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return (Self.scope.resolve(base_env)
                .children_env.get_first(Self.name.symbol).node)


class ForeignDecl(FooNode):
    dest_scope = Field(type=T.Id)
    decl_id = Field(type=T.SimpleId)

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.decl_id.symbol,
            val=Self,
            dest_env=(Self.dest_scope.resolve(Self.parent.children_env)
                      .children_env)
        )
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
