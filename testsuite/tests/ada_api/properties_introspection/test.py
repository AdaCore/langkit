"""
Test that the introspection API works as expected for properties introspection.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.envs import EnvSpec, add_to_env_kv
from langkit.expressions import (AbstractKind, New, Property, Self,
                                 langkit_property)
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    id_bool = Property(lambda id=T.Bool: id, public=True)
    id_int = Property(lambda id=T.Int: id, public=True)
    id_bigint = Property(lambda id=T.BigInt: id, public=True)
    id_char = Property(lambda id=T.Character: id, public=True)
    id_token = Property(lambda id=T.Token: id, public=True)
    id_sym = Property(lambda id=T.Symbol: id, public=True)
    id_unit = Property(lambda id=T.AnalysisUnit: id, public=True)
    id_root_node = Property(lambda id=T.FooNode: id, public=True)
    id_name = Property(lambda id=T.Name: id, public=True)

    # Test for enums
    id_unit_kind = Property(lambda id=T.AnalysisUnitKind: id, public=True)

    # Test for arrays
    id_node_array = Property(lambda id=T.FooNode.entity.array: id, public=True)
    id_bigint_array = Property(lambda id=T.BigInt.array: id, public=True)


class VarDecl(FooNode):
    name = Field()
    value = Field()

    env_spec = EnvSpec(add_to_env_kv(
        key=Self.name.symbol, val=Self
    ))

    @langkit_property(public=True)
    def eval():
        return Self.value.eval


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):

    @langkit_property(public=True, kind=AbstractKind.abstract,
                      return_type=T.Int)
    def eval():
        pass

    @langkit_property(public=True)
    def eval_plus(addend=T.Expr):
        return Self.eval() + addend.eval()


class Addition(Expr):
    lhs = Field()
    rhs = Field()

    @langkit_property()
    def eval():
        return Self.lhs.eval() + Self.rhs.eval()


class Number(Expr):
    token_node = True

    @langkit_property(external=True, uses_entity_info=False, uses_envs=False)
    def eval():
        pass


class Ref(Expr):
    name = Field()

    @langkit_property(public=True)
    def referenced_var_decl():
        return (Self.node_env.get_first(Self.name)
                .cast_or_raise(T.VarDecl))

    @langkit_property()
    def eval():
        return Self.referenced_var_decl.eval()


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.var_decl),
    var_decl=VarDecl('var', g.name, '=', g.expr, ';'),

    expr=Or(Addition(g.expr, '+', g.expr),
            g.atom),
    atom=Or(g.number, g.ref),
    number=Number(Token.Number),
    ref=Ref(g.name),

    name=Name(Token.Identifier),
)
build_and_run(g, ada_main=['main.adb'])

print('Done')
