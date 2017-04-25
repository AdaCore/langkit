"""
Check that the unification of the return type of match expression is correct.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.compiled_types import ASTNode, T, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import If, No, Property, Self
from langkit.parsers import Grammar, Or, Tok

from lexer_example import Token
from utils import emit_and_print_errors


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pred = Property(True)

    foo_1 = Property(
        If(Self.pred, No(T.BarNode), No(T.Literal)),
        public=True
    )


class BarNode(FooNode):
    pass


class Literal(FooNode):
    pass


def lang_def():
    foo_grammar = Grammar('main_rule')
    foo_grammar.add_rules(
        main_rule=Or(
            BarNode(Tok(Token.Example)),
            Literal(Tok(Token.Number)),
        )
    )
    return foo_grammar
emit_and_print_errors(lang_def)
print('Done')
