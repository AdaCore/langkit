"""
Test that foreign nodes in environment metadata are properly rejected.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Struct, Field, T, UserField, env_metadata
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import New, No, Property, Self
from langkit.parsers import Grammar, List, Opt, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):
    pass


@env_metadata
class Metadata(Struct):
    node = UserField(type=FooNode)


class Name(FooNode):
    tok = Field(type=T.TokenType)

    sym = Property(Self.tok.symbol, type=T.SymbolType)
    resolve = Property(Self.parent.node_env.get(Self.sym).at(0),
                       type=T.FooNode.entity)


class Def(FooNode):
    name = Field(type=T.Name)
    ref = Field(type=T.Name)

    env_spec = EnvSpec(
        add_to_env(
            mappings=New(T.env_assoc, key=Self.name.sym, val=Self),
            metadata=New(Metadata, node=Self.ref.then(
                lambda r: r.resolve.el,
                default_val=No(T.FooNode)
            ))
        )
    )


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(grammar.def_rule),
    def_rule=Def(grammar.name, Opt('+', grammar.name)),
    name=Name(Tok(Token.Identifier, keep=True))
)

build_and_run(grammar, 'main.py')
print('')
print('Done')
