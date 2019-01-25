"""
Test that a custom rule can be used to parse an analysis unit.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.parsers import Grammar, List, Opt, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Def(FooNode):
    name = Field()
    args = Field()
    expr = Field()


@abstract
class Expr(FooNode):
    pass


class Literal(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


class ParentExpr(Expr):
    expr = Field()


class Plus(Expr):
    lhs = Field()
    rhs = Field()


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.def_rule),

    name=Name(Token.Identifier),

    def_rule=Def('def', g.name,
                 Opt('(', List(g.name, sep=','), ')'),
                 '=', g.expr),

    expr=Or(Plus(g.expr, '+', g.expr),
            ParentExpr('(', g.expr, ')'),
            Ref(g.name),
            Literal(Token.Number))
)
build_and_run(g, 'main.py', generate_unparser=True)
print('Done')
