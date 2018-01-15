"""
Check that big memoization tables are properly free'd. This used to yield a
stack overflow.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, LongType
from langkit.expressions import langkit_property
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True, memoized=True, return_type=LongType)
    def compute(i=LongType):
        return i + 1


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=Example(Tok(Token.Example)),
)
build_and_run(foo_grammar, ada_main='main.adb')
print('Done')
