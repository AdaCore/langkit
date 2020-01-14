"""
Check that default argument values are correctly handled in the properties
dispach lowering pass.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool, Field, abstract
from langkit.expressions import Not, langkit_property
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Identifier(FooNode):
    token_node = True


@abstract
class Decl(FooNode):
    name = Field(Identifier)

    @langkit_property(public=True)
    def prop(arg=(Bool, False)):
        return arg


class VarDecl(Decl):
    @langkit_property(public=True)
    def prop(arg=(Bool, False)):
        return Not(arg)


class FunDecl(Decl):
    pass


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(Or(grammar.var_decl, grammar.fun_decl)),
    var_decl=VarDecl('var', grammar.identifier),
    fun_decl=FunDecl('def', grammar.identifier),
    identifier=Identifier(Token.Identifier),
)
build_and_run(grammar, 'main.py')
print('')
print('Done')
