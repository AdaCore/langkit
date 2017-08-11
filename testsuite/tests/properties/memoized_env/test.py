from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T, Token as TokenType
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import Self, langkit_property
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


class Ref(FooNode):
    name = Field(type=TokenType)

    @langkit_property(public=True)
    def referenced():
        return Self.referenced_env.env_node

    @langkit_property(memoized=True)
    def referenced_env():
        return Self.node_env.get(Self.name).at(0).children_env


class Block(FooNode):
    name = Field(type=TokenType)
    content = Field(type=Ref.list)

    env_spec = EnvSpec(
        add_env(),
        add_to_env(T.env_assoc.new(key=Self.name.symbol, val=Self),
                   dest_env=Self.node_env),
    )


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Block(
        Tok(Token.Identifier, keep=True),
        Row('(', List(Ref(Tok(Token.Identifier, keep=True))), ')')[1]
    )
)
build_and_run(foo_grammar, 'main.py')
print('Done')
