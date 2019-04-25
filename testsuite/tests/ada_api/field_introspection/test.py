"""
Test that the introspection API works as expected for queries related to syntax
fields.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, AbstractField, Field, NullField, T, abstract
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    name = AbstractField(T.Name)
    value = AbstractField(T.Expr)


class VarDecl(Decl):
    var_kw = Field()
    name = Field()
    value = Field()


class FunDecl(Decl):
    name = Field()
    value = NullField()


class VarKeyword(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Number(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.decl),

    decl=Or(g.var_decl, g.fun_decl),
    var_decl=VarDecl(VarKeyword('var'), g.name, '=', g.expr, ';'),
    fun_decl=FunDecl('def', g.name, ';'),

    expr=Or(g.number, g.ref),
    number=Number(Token.Number),
    ref=Ref(g.name),

    name=Name(Token.Identifier),
)
build_and_run(g, ada_main=['main.adb'])

print('Done')
