from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, TokenType
from langkit.parsers import Grammar, List, Or, Pick

from lexer_example import Token
from utils import build_and_run


class FooNode(ASTNode):
    pass


class Sequence(FooNode.list):
    pass


class Atom(FooNode):
    tok = Field(type=TokenType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=foo_grammar.element,
    element=Or(foo_grammar.sequence, foo_grammar.atom),
    sequence=Pick('(', List(foo_grammar.element, list_cls=Sequence,
                            empty_valid=True), ')'),
    atom=Atom(Token.Identifier),
)

build_and_run(foo_grammar, 'main.py')

print('Done')
