from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    ASTNode, Field, Token as TokenType, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, List, Or, Pick, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class
class FooNode(ASTNode):
    pass


class Sequence(FooNode.list_type()):
    pass


class Atom(FooNode):
    tok = Field(type=TokenType)


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=foo_grammar.element,
    element=Or(foo_grammar.sequence, foo_grammar.atom),
    sequence=Pick('(', List(foo_grammar.element, list_cls=Sequence,
                            empty_valid=True), ')'),
    atom=Atom(Tok(Token.Identifier, keep=True)),
)

build_and_run(foo_grammar, 'main.py')

print('Done')
