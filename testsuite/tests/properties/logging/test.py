"""
Check that instrumentation properties logging is working property.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import (ASTNode, Bool, Field, Struct, T, UserField,
                         env_metadata)
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import New, Self, langkit_property
from langkit.parsers import Grammar, List, Opt

from lexer_example import Token
from utils import build_and_run


@env_metadata
class Metadata(Struct):
    b = UserField(type=Bool)


class FooNode(ASTNode):
    pass


class HasPlus(FooNode):
    enum_node = True
    qualifier = True


class Name(FooNode):
    token_node = True


class Decl(FooNode):
    has_plus = Field()
    name = Field()
    items = Field()

    env_spec = EnvSpec(
        add_to_env_kv(key=Self.name.symbol, val=Self,
                      metadata=New(Metadata, b=Self.has_plus.as_bool))
    )

    @langkit_property(public=True, return_type=T.Ref.entity.array,
                      activate_tracing=True)
    def entity_items():
        return Self.as_entity.items.map(lambda i: i)


class Ref(FooNode):
    name = Field()


fg = Grammar('main_rule')
fg.add_rules(
    main_rule=List(fg.decl),
    decl=Decl(Opt('+').as_bool(HasPlus),
              fg.name,
              '(', fg.ref_list, ')'),
    ref_list=List(fg.ref, empty_valid=True),
    ref=Ref(fg.name),
    name=Name(Token.Identifier),
)
build_and_run(fg, ada_main='main.adb')
print('Done')
