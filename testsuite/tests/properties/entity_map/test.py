"""
Check that map expressions on entity types work properly.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import (ASTNode, BoolType, Field, Struct, UserField, T,
                         env_metadata)
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import New, Self, langkit_property
from langkit.parsers import Grammar, List, Opt, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@env_metadata
class Metadata(Struct):
    b = UserField(type=BoolType)


class FooNode(ASTNode):
    pass


class Decl(FooNode):
    has_plus = Field()
    name = Field()
    items = Field()

    env_spec = EnvSpec(
        add_to_env(mappings=New(T.env_assoc, key=Self.name.symbol, val=Self),
                   metadata=New(Metadata, b=Self.has_plus))
    )

    @langkit_property(public=True, return_type=T.Ref.entity.array)
    def entity_items():
        return Self.as_entity.items.map(lambda i: i)


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True, return_type=Decl.entity)
    def decl():
        return Self.children_env.get(Self.name).at(0).cast_or_raise(Decl)


fg = Grammar('main_rule')
fg.add_rules(
    main_rule=List(fg.decl),
    decl=Decl(Opt('+').as_bool(),
              Tok(Token.Identifier, keep=True),
              '(', fg.ref_list, ')'),
    ref_list=List(fg.ref, empty_valid=True),
    ref=Ref(Tok(Token.Identifier, keep=True)),
)
build_and_run(fg, 'main.py')
print('Done')
