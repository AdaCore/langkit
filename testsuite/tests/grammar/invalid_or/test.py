from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, Token as TokenType
from langkit.parsers import Grammar, List, Or, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


class Element(ASTNode):
    pass


class Sequence(Element.list):
    pass


class Atom(Element):
    tok = Field(type=TokenType)


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=grammar.element,
    element=Or(grammar.atom,
               grammar.sequence),
    sequence=List('(', grammar.element, ')',
                  sep=',',
                  list_cls=Sequence,
                  empty_valid=True),
    atom=Tok(Token.Identifier, keep=True),
)
emit_and_print_errors(grammar)

print('Done')
