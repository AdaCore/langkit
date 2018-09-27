"""
Test that warnings about imprecise type annotations for syntax fields.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, AbstractField, Field, T, abstract
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    name = AbstractField(T.FooNode)  # Warning on this abstract field


class VarDecl(Decl):
    var_kw = Field(type=T.FooNode)  # Warning on this concrete field
    name = Field(type=T.Name)


class FunDecl(Decl):
    name = Field()


class VarKeyword(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True

g = Grammar('main_rule')
g.add_rules(
    main_rule=List(g.decl),
    decl=Or(g.var_decl, g.fun_decl),
    var_decl=VarDecl(VarKeyword('var'), g.name, ';'),
    fun_decl=FunDecl('def', g.name, ';'),
    name=Name(Token.Identifier),
)
emit_and_print_errors(g)

print('Done')
