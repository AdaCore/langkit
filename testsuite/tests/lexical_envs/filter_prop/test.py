from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, LexicalEnvType, T
from langkit.envs import EnvSpec, add_to_env, add_env
from langkit.expressions import (New, Not, Self, SymbolLiteral, Var,
                                 langkit_property)
from langkit.parsers import Grammar, List, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    @langkit_property()
    def filter_prop(env=LexicalEnvType):
        env_name = Var(env.env_node.cast(Decl).then(lambda d: d.name.symbol))
        null_name = Var(SymbolLiteral('null'))
        return Not(env_name.equals(null_name))


class Decl(FooNode):
    name = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.name.symbol, val=Self)),
        add_env()
    )


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get(Self.name.symbol,
                                 sequential=True,
                                 filter_prop=FooNode.filter_prop)

grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(grammar.decl),
    decl=Decl(
        Or(Tok(Token.Identifier, keep=True), Tok(Token.Null)),
        Tok(Token.LPar),
        List(Or(grammar.decl, grammar.ref), empty_valid=True),
        Tok(Token.RPar)
    ),
    ref=Ref(Tok(Token.Identifier, keep=True)),
)
build_and_run(grammar, 'main.py')
print('Done')
