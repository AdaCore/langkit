"""
Check that referencing an environment A from an environment B when both A and
B are nested in a rebindable environment C works as expected: lookups done
from B within a context where C is rebound should return entities from A which
are also in a context where C is rebound.
"""

from langkit.dsl import ASTNode, Annotations, Field, T
from langkit.envs import EnvSpec, add_env, add_to_env_kv, reference
from langkit.expressions import Entity, Self, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    @langkit_property(return_type=T.FooNode.entity, public=True)
    def lookup(sym=T.Symbol):
        return Entity.children_env.get_first(sym)

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def with_rebinding(from_scope=T.Scope.entity, to_scope=T.Scope.entity):
        rbs = Var(Entity.info.rebindings.append_rebinding(
            from_scope.children_env,
            to_scope.children_env
        ))
        e_info = Var(T.entity_info.new(
            md=Entity.info.md,
            rebindings=rbs,
            from_rebound=False,
        ))
        return FooNode.entity.new(node=Self, info=e_info)


class Scope(FooNode):
    name = Field(type=T.Id)
    content = Field()

    annotations = Annotations(rebindable=True)

    @langkit_property(return_type=T.LexicalEnv)
    def refd_envs():
        return Entity.content.children.filtermap(
            lambda c: c.cast(RefClause).resolve,
            lambda c: c.is_a(RefClause)
        ).env_group()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, value=Self),
        add_env(),
        reference(
            nodes=[Self.cast(FooNode)],
            through=T.Scope.refd_envs
        )
    )


class Id(FooNode):
    token_node = True


class RefClause(FooNode):
    ref_id = Field(type=T.Id)

    @langkit_property(return_type=T.LexicalEnv)
    def resolve():
        return Entity.lookup(Self.ref_id.symbol)._.children_env


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
