"""
Check that "==" and "!=" operators for Entity in the Python API behave as
expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract, has_abstract_list
from langkit.envs import EnvSpec, add_env, add_to_env
from langkit.expressions import (AbstractProperty, Entity, New, Property, Self,
                                 T, Var, langkit_property)
from langkit.parsers import Grammar, List, Pick

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


@abstract
class DefNode(FooNode):
    name = AbstractProperty(T.Symbol, public=True)
    env_spec = EnvSpec(add_to_env(mappings=New(T.env_assoc,
                                               key=Self.name,
                                               val=Self)))


class Block(DefNode):
    params = Field(type=T.Params)
    name_field = Field(type=T.Name)
    vars = Field(type=T.BlockVar.list)

    name = Property(Self.name_field.symbol)

    @langkit_property(public=True)
    def rebind(from_block=T.Block.entity, to_block=T.Block.entity):
        rbdng = Var(Entity.info.rebindings.append_rebinding(
            from_block.params.children_env,
            to_block.children_env
        ))

        e_info = Var(New(T.entity_info,
                         md=Entity.info.md,
                         rebindings=rbdng,
                         from_rebound=False))

        return New(Block.entity, node=Self, info=e_info)

    env_spec = EnvSpec(
        add_to_env(mappings=New(T.env_assoc, key=Self.name, val=Self)),
        add_env()
    )


@has_abstract_list
class Param(DefNode):
    name_field = Field(type=T.Name)
    name = Property(Self.name_field.symbol)


class Params(Param.list):
    env_spec = EnvSpec(add_env())


class BlockVar(DefNode):
    name_field = Field(type=T.Name)
    name = Property(Self.name_field.symbol)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(grammar.block),
    name=Name(Token.Identifier),
    block=Block(grammar.params, grammar.name, grammar.vars),

    params=Pick('(', List(grammar.param, list_cls=Params), ')'),
    param=Param(grammar.name),

    vars=Pick('{', List(grammar.var), '}'),
    var=BlockVar(grammar.name),
)
build_and_run(grammar, 'main.py')
print('Done')
