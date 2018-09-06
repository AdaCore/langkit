"""
Test basic features for enumeration types.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Enum, EnumValue, Field, abstract
from langkit.expressions import AbstractProperty, Property, langkit_property
from langkit.parsers import Grammar, List, Or, Pick

from lexer_example import Token
from utils import build_and_run


class DeclKind(Enum):
    func = EnumValue()
    var = EnumValue()


class FooNode(ASTNode):

    @langkit_property(public=True)
    def identity(k=DeclKind):
        return k


@abstract
class Decl(FooNode):
    decl_kind = AbstractProperty(DeclKind, public=True)


class VarDecl(Decl):
    name = Field()
    expr = Field()

    decl_kind = Property(DeclKind.var)


class FuncDecl(Decl):
    name = Field()
    args = Field()
    expr = Field()

    decl_kind = Property(DeclKind.func)


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    token_node = True


class Name(Expression):
    token_node = True


class Plus(Expression):
    left = Field()
    right = Field()


g = Grammar('main_rule')
g.add_rules(
    main_rule=List(Pick(g.decl, ';')),
    decl=Or(VarDecl('def', g.name, '=', g.expr),
            FuncDecl('def', g.name,
                     '(', List(g.name, sep=','), ')',
                     '=', g.expr)),
    expr=Or(Plus(g.atom, '+', g.expr),
            g.atom),
    atom=Or(Literal(Token.Number), g.name),
    name=Name(Token.Identifier),
)
build_and_run(g, 'main.py')
print('Done')
