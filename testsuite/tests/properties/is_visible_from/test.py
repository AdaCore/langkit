"""
Test that the "is_visible_from" operation properly raises a PropertyError for
invalid input.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, BoolType, Field, T
from langkit.envs import EnvSpec, add_to_env, add_env
from langkit.expressions import EmptyEnv, If, New, Self, Var, langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def prop(empty1=BoolType, empty2=BoolType):
        arg1 = Var(If(empty1, EmptyEnv, Self.children_env))
        arg2 = Var(If(empty2, EmptyEnv, Self.children_env))
        return arg1.is_visible_from(arg2)


class Name(FooNode):
    tok = Field()

    env_spec = EnvSpec(
        add_to_env(New(T.env_assoc, key=Self.tok.symbol, val=Self)),
        add_env()
    )


class Scope(Name.list):
    env_spec = EnvSpec(add_env())


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.name, list_cls=Scope),
    name=Name(Tok(Token.Identifier, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
