"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

import os.path

from langkit.compiled_types import ASTNode, Field, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import New, Property, Self, langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import emit_and_print_errors


def run(name, prop):
    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))
    print('== {} =='.format(name))

    @root_grammar_class()
    class FooNode(ASTNode):
        resolve_ref = prop

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
            resolver=FooNode.fields.resolve_ref
        ))

        @langkit_property(public=True)
        def resolve():
            return Self.node_env.get(Self.name.symbol).at(0)

    def lang_def():
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
        return foo_grammar
    emit_and_print_errors(lang_def)

run('Bad return type', Property(Self.node_env.get('foo')))
run('Has implicit env', Property(Self.node_env.get('foo').at(0),
                                 has_implicit_env=True))
print('Done')
