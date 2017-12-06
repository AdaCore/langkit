from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import PropertyError, langkit_property
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def raise_msg():
        return PropertyError(T.BoolType, 'Custom error message!')

    @langkit_property(public=True)
    def raise_no_msg():
        return PropertyError(T.BoolType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Example)),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
