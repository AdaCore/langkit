from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import (
    ASTNode, Field, Token as TokenType, root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.parsers import Grammar, List, Or, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class
class Element(ASTNode):
    pass


class Sequence(Element.list_type()):
    pass


class Atom(Element):
    tok = Field(type=TokenType)


def grammar():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=foo_grammar.element,
        element=Or(foo_grammar.atom,
                   foo_grammar.sequence),
        sequence=List('(', foo_grammar.element, ')',
                      sep=',',
                      list_cls=Sequence,
                      empty_valid=True),
        atom=Tok(Token.Identifier, keep=True),
    )
    return foo_grammar

emit_and_print_errors(grammar)

print('Done')
