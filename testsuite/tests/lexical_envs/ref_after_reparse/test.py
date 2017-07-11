"""
Test that the referenced envs link that cross analysis units stay valid after
reparsing.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, LexicalEnvType, T
from langkit.envs import EnvSpec, reference, add_to_env, add_env
from langkit.expressions import DynamicVariable, New, Self, langkit_property
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
Env = DynamicVariable('env', LexicalEnvType)


class FooNode(ASTNode):
    pass


class Name(FooNode):
    tok = Field()

    @langkit_property()
    def sym():
        return Self.tok.symbol

    @langkit_property(dynamic_vars=[Env])
    def ambiant_entity():
        return Env.get(Self.sym).at(0)

    @langkit_property()
    def designated_env():
        return Self.unit.root.node_env.get(Self.sym).at(0).children_env

    @langkit_property(public=True)
    def entity():
        return Env.bind(Self.node_env, Self.ambiant_entity)


class Block(FooNode):
    name = Field()
    decls = Field()
    usings = Field()
    refs = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.name.sym, val=Self)),
        add_env()
    )


class Decl(FooNode):
    name = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.name.sym, val=Self))
    )


class Using(FooNode):
    name = Field()
    env_spec = EnvSpec(
        reference(Self.name.cast(FooNode).to_array,
                  through=Name.designated_env)
    )


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True)
    def entity():
        return Self.as_entity.name.entity


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.block),

    name=Name(Tok(Token.Identifier, keep=True)),

    block=Block(foo_grammar.name,
                foo_grammar.decl_list,
                Tok(Token.LBrace),
                foo_grammar.using_list,
                foo_grammar.ref_list,
                Tok(Token.RBrace)),

    decl_list=Row(Tok(Token.LPar),
                  List(foo_grammar.decl, empty_valid=True),
                  Tok(Token.RPar))[1],

    using_list=Row(Tok(Token.LPar),
                   List(foo_grammar.using, empty_valid=True),
                   Tok(Token.RPar))[1],

    ref_list=List(foo_grammar.ref, empty_valid=True),

    decl=Decl(foo_grammar.name),
    using=Using(foo_grammar.name),
    ref=Ref(foo_grammar.name),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
