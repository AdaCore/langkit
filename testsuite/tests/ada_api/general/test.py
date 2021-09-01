"""
Check that the Children_And_Trivia function works as expected.
"""

from langkit.dsl import (
    ASTNode, Field, UserField, env_metadata, Struct, Bool, T
)
from langkit.expressions import langkit_property, Entity

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    dummy = UserField(Bool)


class FooNode(ASTNode):

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def set_dummy():
        """
        Return an entity for Self with some metadata set. Used to check
        equality in the Ada API (see main.adb).
        """
        return T.FooNode.entity.new(
            node=Entity.node,
            info=T.entity_info.new(
                md=Metadata.new(
                    dummy=True
                ),
                rebindings=Entity.info.rebindings,
                from_rebound=Entity.info.from_rebound
            )
        )


class DeclError(FooNode):
    enum_node = True
    qualifier = True


class Identifier(FooNode):
    token_node = True


class Decl(FooNode):
    id = Field(type=Identifier)
    error = Field(type=DeclError)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main='main.adb',
              types_from_lkt=False)
print('Done')
