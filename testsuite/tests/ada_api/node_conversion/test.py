"""
Test that the introspection API works as expected for queries related to node
types.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class VarDecl(FooNode):
    name = Field()
    value = Field()


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Addition(Expr):
    lhs = Field()
    rhs = Field()


class Number(Expr):
    token_node = True


class Null(FooNode):
    enum_node = True
    qualifier = True


class Ref(Expr):
    null_qual = Field()
    name = Field()


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.var_decl),
    var_decl=VarDecl('var', g.name, '=', g.expr, ';'),

    expr=Or(Addition(g.expr, '+', g.expr),
            g.atom),
    atom=Or(g.number, g.ref),
    number=Number(Token.Number),
    ref=Ref(Null('null'), g.name),

    name=Name(Token.Identifier),
)
build_and_run(g, ada_main=['main.adb'])

print('Done')
