"""
Test that the ".p_node_env" property returns EmptyEnv even when the node's
self-env is also EmptyEnv. It used to return null, which is an erroneous
environment value in the DSL.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, do
from langkit.expressions import Entity, PropertyError, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Decl(FooNode):
    names = Field()
    env_spec = EnvSpec(do(PropertyError(T.FooNode)), add_env())

    @langkit_property(public=True)
    def lookup(n=T.Name.entity):
        return Entity.node_env.get_first(n.symbol)


class Name(FooNode):
    token_node = True


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
