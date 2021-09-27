"""
Check that set_initial_env actions that fetch a foreign environment without the
unsafe=True flag raise an error.
"""

from langkit.dsl import (ASTNode, Field, Struct, T, UserField, abstract,
                         env_metadata)
from langkit.envs import EnvSpec, add_env, add_to_env_kv, set_initial_env
from langkit.expressions import (AbstractKind, Self, direct_env,
                                 langkit_property)

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    node = UserField(T.FooNode)


class FooNode(ASTNode):
    pass


class Scope(FooNode):
    name = Field(type=T.Id)
    content = Field()

    env_spec = EnvSpec(
        set_initial_env(
            direct_env(Self.name.designated_scope(Self.parent.children_env))
        ),
        add_to_env_kv(key=Self.name.designated_symbol, val=Self),
        add_env(),
    )


@abstract
class Id(FooNode):
    @langkit_property(return_type=T.Symbol, kind=AbstractKind.abstract)
    def designated_symbol():
        pass

    @langkit_property(return_type=T.LexicalEnv, kind=AbstractKind.abstract)
    def designated_scope(base_env=T.LexicalEnv):
        pass

    @langkit_property(return_type=T.FooNode, kind=AbstractKind.abstract)
    def resolve(base_env=T.LexicalEnv):
        pass


class SimpleId(Id):
    token_node = True

    @langkit_property()
    def designated_symbol():
        return Self.symbol

    @langkit_property()
    def designated_scope(base_env=T.LexicalEnv):
        return base_env

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return base_env.get_first(Self.symbol).node


class ScopedId(Id):
    scope = Field(type=T.Id)
    name = Field(type=T.SimpleId)

    @langkit_property()
    def designated_symbol():
        return Self.name.symbol

    @langkit_property()
    def designated_scope(base_env=T.LexicalEnv):
        return Self.scope.resolve(base_env).children_env

    @langkit_property()
    def resolve(base_env=T.LexicalEnv):
        return (Self.designated_scope(base_env).get_first(Self.name).node)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
