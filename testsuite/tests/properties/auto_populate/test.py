"""
Check that Populate_Lexical_Env is automatically called in public properties.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import Entity, New, Self, langkit_property
from langkit.parsers import Grammar, List, Tok

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Decl(FooNode):
    name = Field()
    items = Field()

    env_spec = EnvSpec(
        add_to_env(mappings=New(T.env_assoc, key=Self.name.symbol, val=Self))
    )


class Ref(FooNode):
    name = Field()

    @langkit_property(public=True, return_type=Decl.entity)
    def decl_wrapper():
        return Entity.decl

    @langkit_property(public=True, return_type=Decl.entity)
    def decl():
        return Self.children_env.get_first(Self.name).cast_or_raise(Decl)


fg = Grammar('main_rule')
fg.add_rules(
    main_rule=List(fg.decl),
    decl=Decl(Tok(Token.Identifier, keep=True),
              '(', fg.ref_list, ')'),
    ref_list=List(fg.ref, empty_valid=True),
    ref=Ref(Tok(Token.Identifier, keep=True)),
)
build_and_run(fg, 'main.py')
print('Done')
