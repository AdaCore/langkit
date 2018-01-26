"""
Test getting the analysis unit of a node in the properties DSL.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.expressions import (
    Property, Self
)
from langkit.parsers import Grammar, Or

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    root_node = Property(Self.unit.root.as_bare_entity, public=True)


@abstract
class Expression(FooNode):
    pass


class Literal(Expression):
    token_node = True


class Plus(Expression):
    left = Field()
    right = Field()


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Or(
        Plus(foo_grammar.atom, '+', foo_grammar.main_rule),
        foo_grammar.atom
    ),
    atom=Literal(Token.Number),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
