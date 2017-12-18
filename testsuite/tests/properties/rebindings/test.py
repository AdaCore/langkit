"""
Check that .rebindings_* DSL operations behave as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import (AbstractProperty, Entity, New, Property, Self,
                                 T, Var, langkit_property)
from langkit.parsers import Grammar, List, Pick, Tok

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Def(FooNode):
    name = AbstractProperty(T.SymbolType, public=True)
    env_spec = EnvSpec(add_to_env(mappings=New(T.env_assoc,
                                               key=Self.name,
                                               val=Self)))


class Block(Def):
    params = Field(type=T.Params)
    tok = Field(type=T.TokenType)
    vars = Field(type=T.BlockVar.list)

    name = Property(Self.tok.symbol)

    @langkit_property(public=True)
    def rebind(from_block=T.Block.entity, to_block=T.Block.entity):
        rbdng = Var(Entity.info.rebindings.append_rebinding(
            from_block.params.children_env,
            to_block.children_env
        ))

        e_info = Var(New(T.entity_info,
                         md=Entity.info.md,
                         rebindings=rbdng))

        return New(Block.entity, el=Self, info=e_info)

    @langkit_property(public=True)
    def parent_rebindings():
        e_info = Var(New(T.entity_info,
                         md=Entity.info.md,
                         rebindings=Entity.info.rebindings.rebindings_parent))
        return New(Block.entity, el=Self, info=e_info)

    @langkit_property(public=True)
    def new():
        return (Entity.info.rebindings.rebindings_new_env.env_node
                .as_bare_entity)

    @langkit_property(public=True)
    def old():
        return (Entity.info.rebindings.rebindings_new_env.env_node
                .as_bare_entity)

    env_spec = EnvSpec(
        add_to_env(mappings=New(T.env_assoc, key=Self.name, val=Self)),
        add_env()
    )


class Param(Def):
    tok = Field(type=T.TokenType)
    name = Property(Self.tok.symbol)


class Params(Param.list):
    env_spec = EnvSpec(add_env())


class BlockVar(Def):
    tok = Field(type=T.TokenType)
    name = Property(Self.tok.symbol)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(grammar.block),
    block=Block(grammar.params,
                Tok(Token.Identifier, keep=True),
                grammar.vars),

    params=Pick('(', List(grammar.param, list_cls=Params), ')'),
    param=Param(Tok(Token.Identifier, keep=True)),

    vars=Pick('{', List(grammar.var), '}'),
    var=BlockVar(Tok(Token.Identifier, keep=True)),
)
build_and_run(grammar, 'main.py')
print('Done')
