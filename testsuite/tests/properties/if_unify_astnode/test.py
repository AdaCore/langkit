"""
Check that the unification of the return type of match expression is correct.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T
from langkit.expressions import If, No, Property, Self
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pred = Property(True)

    foo_1 = Property(
        If(Self.pred, No(T.BarNode), No(T.Literal)).as_bare_entity,
        public=True
    )


class BarNode(FooNode):
    pass


class Literal(FooNode):
    pass


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=Or(
        BarNode(Tok(Token.Example)),
        Literal(Tok(Token.Number)),
    )
)
emit_and_print_errors(grammar)
print('Done')
