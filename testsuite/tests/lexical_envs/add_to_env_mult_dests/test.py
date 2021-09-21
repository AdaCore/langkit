"""
Test add_to_env with multiple env_assoc having different destination
environments.
"""

from langkit.dsl import (ASTNode, Field, LookupKind, Struct, T, UserField,
                         env_metadata)
from langkit.envs import EnvSpec, add_env, add_to_env, handle_children
from langkit.expressions import (Self, direct_env, langkit_property,
                                 new_env_assoc)

from utils import build_and_run


@env_metadata
class Metadata(Struct):
    node = UserField(T.FooNode)


class FooNode(ASTNode):
    pass


class Scope(FooNode):
    name = Field(type=T.Id)
    content = Field(type=T.FooNode.list)

    env_spec = EnvSpec(
        add_env(),
        handle_children(),
        add_to_env(
            Self.content.children.map(
                lambda r: new_env_assoc(
                    key="Scope",
                    val=Self,
                    dest_env=direct_env(r.match(
                        lambda s=T.Scope: s.name.children_env,
                        lambda _: r.children_env
                    )),
                )
            )
        ),
    )


class Id(FooNode):
    token_node = True

    @langkit_property(return_type=T.FooNode.entity, public=True)
    def get_scope():
        return Self.children_env.get_first("Scope", lookup=LookupKind.minimal)

    env_spec = EnvSpec(add_env())


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py')
print('Done')
