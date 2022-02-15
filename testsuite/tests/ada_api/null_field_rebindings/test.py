"""
Test in the public Ada API that the access of a null field from a node that has
rebindings is truly null, i.e. that field access returns a null node *and* null
metadata.
"""

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_env
from langkit.expressions import Entity, Self, Var, langkit_property

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):
    token_node = True

    env_spec = EnvSpec(add_env())


class Identifier(FooNode):
    token_node = True


class Decl(FooNode):
    name = Field(T.Identifier)

    @langkit_property(public=True)
    def rebind(from_node=T.FooNode.entity, to_node=T.FooNode.entity):
        rbdng = Var(Entity.info.rebindings.append_rebinding(
            from_node.children_env,
            to_node.children_env
        ))

        e_info = Var(T.entity_info.new(
            md=Entity.info.md,
            rebindings=rbdng,
            from_rebound=False
        ))

        return T.Decl.entity.new(node=Self, info=e_info)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
