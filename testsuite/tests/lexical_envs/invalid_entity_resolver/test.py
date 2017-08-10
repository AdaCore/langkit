"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, LexicalEnvType, T
from langkit.envs import EnvSpec, add_to_env, add_env
from langkit.expressions import (DynamicVariable, New, Property, Self,
                                 langkit_property)
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnvType)


def run(name, prop):
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        resolve_ref = prop

    class Decl(FooNode):
        name = Field()
        refs = Field()

        env_spec = EnvSpec(
            add_to_env(
                New(T.env_assoc, key=Self.name.symbol, val=Self)
            ),
            add_env()
        )

    class Ref(FooNode):
        name = Field()

        env_spec = EnvSpec(add_to_env(
            New(T.env_assoc, key=Self.name.symbol, val=Self),
            resolver=FooNode.resolve_ref
        ))

        @langkit_property(public=True)
        def resolve():
            return Self.node_env.get(Self.name.symbol).at(0)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=List(grammar.decl),
        decl=Decl(
            Tok(Token.Identifier, keep=True),
            Tok(Token.LPar),
            List(grammar.ref, empty_valid=True),
            Tok(Token.RPar)
        ),
        ref=Ref(Tok(Token.Identifier, keep=True)),
    )
    emit_and_print_errors(grammar)

run('Bad return type', Property(Self.node_env.get('foo')))
run('Has dynamic variable', Property(Self.node_env.get('foo').at(0),
                                     dynamic_vars=[Env]))
print('Done')
