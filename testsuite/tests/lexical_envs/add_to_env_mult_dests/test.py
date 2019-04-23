"""
Test add_to_env with multiple env_assoc having different destination
environments.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import (ASTNode, Field, Struct, T, UserField, abstract,
                         env_metadata, LookupKind)
from langkit.envs import EnvSpec, add_to_env, add_env, handle_children
from langkit.expressions import (
    AbstractKind, New, Self, langkit_property, No, ArrayLiteral as Array,
    new_env_assoc
)
from langkit.parsers import Grammar, List, Opt, Or

from lexer_example import Token
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
                    dest_env=r.match(
                        lambda s=T.Scope: s.name.children_env,
                        lambda _: r.children_env
                    )
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


G = Grammar('main_rule')
G.add_rules(
    main_rule=G.elem,
    elem=Or(G.scope, G.id),
    scope=Scope(G.id, '{', List(G.elem), '}'),
    id=Id(Token.Identifier)
)

build_and_run(G, 'main.py')
print('Done')
