"""
Test that invalid uses of abstract fields are duly diagnosed and rejected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, AbstractField, Field, T, abstract
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    name = AbstractField(T.Name)


class VarDecl(Decl):
    var_kw = Field()
    name = Field()


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
build_and_run(g, ada_main=['main.adb'])

print('Done')
