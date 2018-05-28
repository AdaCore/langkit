"""
Check that having private properties used as predicates in the grammar
generates valid Ada code.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode
from langkit.expressions import Not, Self, langkit_property
from langkit.parsers import Grammar, Predicate

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True

    @langkit_property()
    def is_not_class_id():
        return Not(Self.symbol == 'class')


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Predicate(Name(Token.Identifier), Name.is_not_class_id),
)
build_and_run(foo_grammar, 'main.py')
print('Done')
