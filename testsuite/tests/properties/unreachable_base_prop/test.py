"""
Test that we properly warn for properties that are unreachable because they are
defined on an abstract node while all concrete subclasses have it overriden.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.expressions import AbstractProperty, Property
from langkit.parsers import Grammar, List, Or, Pick

from lexer_example import Token
from utils import emit_and_print_errors


@abstract
class FooNode(ASTNode):
    pass


@abstract
class Expr(FooNode):

    prop1 = AbstractProperty(T.LongType, public=True)

    # Warning: all concrete subclasses override this (concrete root property)
    prop2 = Property(0, public=True)

    # Warning: all concrete subclasses override this (runtime check)
    prop3 = AbstractProperty(T.LongType, public=True, runtime_check=True)


@abstract
class Atom(Expr):

    # Warning: both concrete subclasses override this (concrete derived
    # property).
    prop1 = Property(1)


class Lit(Atom):
    tok = Field()

    prop1 = Property(2)
    prop2 = Property(2)
    prop3 = Property(2)


class Ref(Atom):
    tok = Field()

    prop1 = Property(3)
    prop2 = Property(3)
    prop3 = Property(3)


class Plus(Expr):
    lhs = Field()
    rhs = Field()

    prop1 = Property(4)
    prop2 = Property(4)
    prop3 = Property(4)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(grammar.expr),

    expr=Or(grammar.atom, grammar.plus),

    atom=Or(grammar.lit, grammar.ref),
    lit=Lit(Token.Number),
    ref=Ref(Token.Identifier),

    plus=Pick('(', Plus(grammar.expr, '+', grammar.expr), ')'),
)
emit_and_print_errors(grammar)
print('Done')
