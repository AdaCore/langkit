"""
Check that the DSL equality operator tests for equivalence, not pointer
equality.
"""

from langkit.dsl import ASTNode, Field, Struct, T, UserField
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import New, Self, langkit_property

from utils import build_and_run


class EnvStruct(Struct):
    env = UserField(type=T.LexicalEnv)


class FooNode(ASTNode):

    @langkit_property()
    def env_struct():
        return New(EnvStruct, env=Self.children_env.env_orphan)

    @langkit_property()
    def env_array():
        return Self.children_env.env_orphan.singleton


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
        add_to_env_kv(key=Self.name.symbol, val=Self),
        add_env(),
    )

    @langkit_property(public=True, return_type=T.Bool)
    def test_env(other=T.FooNode.entity):
        return Self.children_env.env_orphan == other.children_env.env_orphan

    @langkit_property(public=True, return_type=T.Bool)
    def test_struct(other=T.FooNode.entity):
        return Self.env_struct == other.env_struct

    @langkit_property(public=True, return_type=T.Bool)
    def test_array(other=T.FooNode.entity):
        return Self.env_array == other.env_array


class Ref(FooNode):
    name = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',

              # FIXME: switch back to True, see U920-003
              lkt_semantic_checks=False)
print('Done')
