"""
Test that:

 1. It is possible to call an AST node property on the corresponding entity.

 2. When accessing an AST node field through the corresponding entity, this
    returns the component AST node wrapped in an entity with the same entity
    info.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import (ASTNode, BoolType, EnvRebindingsType, Field, Struct,
                         T, UserField, env_metadata)
from langkit.expressions import New, No, Self, langkit_property
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class FooNode(ASTNode):

    @langkit_property(public=True)
    def test_main():
        return (
            Self.cast(T.Couple)

            # This will return an entity with is_first=True
            .first_entity

            # This is supposed to forward the entity metadata from the previous
            # line.
            .first
        )


@env_metadata
class Metadata(Struct):
    is_first = UserField(BoolType)
    is_second = UserField(BoolType)


class Literal(FooNode):
    tok = Field()


class Couple(FooNode):
    first = Field()
    second = Field()

    @langkit_property()
    def first_entity():
        return Self.as_entity.get_entity(
            New(Metadata, is_first=True, is_second=False)
        )

    @langkit_property()
    def get_entity(md=Metadata):
        return New(Couple.entity,
                   el=Self,
                   info=New(T.entity_info,
                            md=md,
                            rebindings=No(EnvRebindingsType)))

    @langkit_property(public=True)
    def property_on_entity():
        # Check that we can do ".get_entity" even when the prefix is an entity
        return Self.as_entity.get_entity(Self.as_entity.info.md)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=foo_grammar.item,
    item=Or(foo_grammar.couple, foo_grammar.literal),
    couple=Couple('(', foo_grammar.item, ',', foo_grammar.item, ')'),
    literal=Literal(Tok(Token.Number, keep=True)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
