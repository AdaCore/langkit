"""
Test that foreign nodes in environment metadata are properly rejected.
"""

from langkit.dsl import ASTNode, Field, Struct, T, UserField, env_metadata
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import New, No, Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


@env_metadata
class Metadata(Struct):
    node = UserField(type=FooNode)


class Name(FooNode):
    token_node = True

    sym = Property(Self.symbol, type=T.Symbol)
    resolve = Property(Self.parent.node_env.get(Self.sym).at(0),
                       type=T.FooNode.entity)


class Def(FooNode):
    name = Field(type=T.Name)
    ref = Field(type=T.Name)

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.name.sym, val=Self,
            metadata=New(Metadata, node=Self.ref.then(
                lambda r: r.resolve.node,
                default_val=No(T.FooNode)
            ))
        )
    )


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('')
print('Done')
