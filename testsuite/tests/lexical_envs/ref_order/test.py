"""
Check that when declaring a sequence of referenced envs, the resolver of each
env can only depend on referenced envs added *before* itself. Thanks to this
property, also check that a certain kind of lookup that used to take forever
now completes instantaneously.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import Entity, Self, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(return_type=T.FooNode.entity, public=True)
    def lookup(sym=T.Symbol):
        return Entity.children_env.get_first(sym)

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def with_rebindings():
        rbs = Var(Entity.info.rebindings.append_rebinding(
            Self.children_env,
            Self.children_env
        ))
        e_info = Var(T.entity_info.new(
            md=Entity.info.md,
            rebindings=rbs,
            from_rebound=Entity.info.from_rebound,
        ))
        return FooNode.entity.new(node=Self, info=e_info)


class Scope(FooNode):
    name = Field(type=T.Id)
    content = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env(),
    )


class Id(FooNode):
    token_node = True


class RefClause(FooNode):
    ref_id = Field(type=T.Id)

    @langkit_property(return_type=T.LexicalEnv)
    def resolve():
        return Entity.lookup(Self.ref_id.symbol)._.children_env

    env_spec = EnvSpec(
        reference(
            nodes=[Self.cast(FooNode)],
            through=T.RefClause.resolve
        )
    )


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
