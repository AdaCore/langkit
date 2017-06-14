"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import New, No, Self, langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):

    @langkit_property()
    def resolve_ref():
        return Self.match(
            lambda r=T.Ref: r.parent.parent.node_env.get(r.name.symbol).at(0),
            lambda _: No(T.entity),
        )


class Decl(FooNode):
    name = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_env=True,
        add_to_env=add_to_env(
            New(T.env_assoc, key=Self.name.symbol, val=Self)
        )
    )


class Ref(FooNode):
    name = Field()

    env_spec = EnvSpec(add_to_env=add_to_env(
        New(T.env_assoc, key=Self.name.symbol, val=Self),
        resolver=FooNode.resolve_ref
    ))

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get(Self.name.symbol).at(0)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.decl),
    decl=Decl(
        Tok(Token.Identifier, keep=True),
        Tok(Token.LPar),
        List(foo_grammar.ref, empty_valid=True),
        Tok(Token.RPar)
    ),
    ref=Ref(Tok(Token.Identifier, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
