"""
Check that uses of stale references to rebindings in the Ada API are rejected.
"""

from langkit.dsl import ASTNode
from langkit.envs import EnvSpec, add_env
from langkit.expressions import Entity, New, Self, T, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property(public=True, return_type=T.FooNode.entity)
    def rebind(from_node=T.FooNode.entity, to_node=T.FooNode.entity):
        rbdng = Var(Entity.info.rebindings.append_rebinding(
            from_node.children_env,
            to_node.children_env
        ))

        e_info = Var(New(T.entity_info,
                         md=Entity.info.md,
                         rebindings=rbdng,
                         from_rebound=False))

        return New(T.FooNode.entity, node=Self, info=e_info)


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(add_env())


build_and_run(lkt_file="expected_concrete_syntax.lkt", ada_main="main.adb")
print('Done')
