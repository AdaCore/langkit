from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, has_abstract_list
from langkit.parsers import Grammar, List, Or

from lexer_example import Token
from utils import emit_and_print_errors


@has_abstract_list
class Element(ASTNode):
    pass


class Sequence(Element.list):
    pass


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=grammar.element,
    element=Or(grammar.atom,
               grammar.sequence),
    sequence=List('(', grammar.element, ')',
                  sep=',',
                  list_cls=Sequence,
                  empty_valid=True),
    atom=Token.Identifier,
)
emit_and_print_errors(grammar)

print('Done')
