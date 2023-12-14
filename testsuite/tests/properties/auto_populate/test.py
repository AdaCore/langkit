"""
Check that Populate_Lexical_Env is automatically called in public properties.
"""

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import Entity, Self, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Decl(FooNode):
    name = Field()
    items = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self)
    )


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True, return_type=Decl.entity)
    def decl_wrapper():
        return Entity.decl

    @langkit_property(public=True, return_type=Decl.entity)
    def decl():
        return Self.children_env.get_first(Self.name).cast_or_raise(Decl)


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
