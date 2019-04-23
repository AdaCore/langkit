"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_to_env_kv, add_env
from langkit.expressions import New, No, Self, langkit_property
from langkit.parsers import Grammar, List

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):

    @langkit_property()
    def resolve_ref():
        return Self.match(
            lambda r=T.Ref: r.parent.parent.node_env.get(r.name).at(0),
            lambda _: No(T.entity),
        )


class Name(FooNode):
    token_node = True


class Decl(FooNode):
    name = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_to_env_kv(
            key=Self.name.symbol, val=Self
        ),
        add_env()
    )


class Ref(FooNode):
    name = Field()

    env_spec = EnvSpec(add_to_env_kv(
        key=Self.name.symbol, val=Self,
        resolver=FooNode.resolve_ref
    ))

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get(Self.name).at(0)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.decl),
    decl=Decl(
        Name(Token.Identifier),
        '(', List(foo_grammar.ref, empty_valid=True), ')'
    ),
    ref=Ref(Name(Token.Identifier)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
