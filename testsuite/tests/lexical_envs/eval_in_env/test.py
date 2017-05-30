"""
Test the handling of analysis units in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import ASTNode, Field, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import Env, New, Self, langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


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
    ))

    @langkit_property(public=True)
    def resolve():
        return Self.parent.parent.node_env.eval_in_env(
            Env.get(Self.name.symbol).at(0)
        )


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
