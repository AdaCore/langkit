"""
Check that .rebindings related built-in properties behave as expected.
"""

from langkit.dsl import ASTNode, Annotations, Field
from langkit.envs import EnvSpec, add_env
from langkit.expressions import Entity, Self, T, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Block(FooNode):
    name = Field(type=T.Name)
    inner = Field(type=T.Block.list)

    env_spec = EnvSpec(add_env())

    annotations = Annotations(rebindable=True)

    @langkit_property()
    def create_entity(rbdng=T.EnvRebindings):
        """
        Helper to create a new entity from ``Entity`` but with the given
        rebindings.
        """
        e_info = Var(
            T.entity_info.new(
                md=Entity.info.md,
                rebindings=rbdng,
                from_rebound=False,
            )
        )
        return Block.entity.new(node=Self, info=e_info)

    @langkit_property(public=True)
    def rebind(from_block=T.Block.entity, to_block=T.Block.entity):
        return Entity.create_entity(
            Entity.info.rebindings.append_rebinding(
                from_block.children_env,
                to_block.children_env
            )
        )

    @langkit_property(public=True)
    def concat_rebindings(other=T.Block.entity):
        return Entity.create_entity(Entity.info.rebindings.concat_rebindings(
            other.info.rebindings
        ))

    @langkit_property(public=True)
    def shed_rebindings(from_block=T.Block.entity):
        return Entity.create_entity(from_block.children_env.shed_rebindings(
            Entity.info
        ).rebindings)

    # Accessors so that main.py can explore the rebindings chain
    @langkit_property(public=True)
    def parent_rebindings():
        return Entity.create_entity(Entity.info.rebindings.get_parent)

    @langkit_property(public=True)
    def new():
        return Entity.info.rebindings.new_env.env_node.as_bare_entity

    @langkit_property(public=True)
    def old():
        return Entity.info.rebindings.old_env.env_node.as_bare_entity


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
