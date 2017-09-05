from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, Field, LexicalEnvType, T
from langkit.envs import EnvSpec, add_to_env, add_env
from langkit.expressions import (DynamicVariable, New, Property, Self,
                                 langkit_property)
from langkit.parsers import Grammar, List, Or, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Env = DynamicVariable('env', LexicalEnvType)


def run(name, prop, filter_prop=lambda foo_node: foo_node.filter_prop):
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
    print('== {} =='.format(name))

    class FooNode(ASTNode):
        filter_prop = prop

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
            return Self.node_env.get_sequential(
                symbol=Self.name.symbol,
                sequential_from=Self,
                filter_prop=filter_prop(FooNode)
            ).at(0)

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=List(grammar.decl),
        decl=Decl(
            Or(Tok(Token.Identifier, keep=True), Tok(Token.Null)),
            Tok(Token.LPar),
            List(Or(grammar.ref, grammar.decl), empty_valid=True),
            Tok(Token.RPar)
        ),
        ref=Ref(Tok(Token.Identifier, keep=True)),
    )
    emit_and_print_errors(grammar)

run('Bad property reference', Property(Self), filter_prop=lambda _: 1)
run('Bad return type', Property(lambda env=LexicalEnvType: env))
run('Bad argument count', Property(lambda _=LexicalEnvType, b=BoolType: b))
run('Bad argument type', Property(lambda b=BoolType: b))
run('Has dynamic variable', Property(
    lambda env=LexicalEnvType: env.is_visible_from(Env),
    dynamic_vars=[Env]
))
print('Done')
