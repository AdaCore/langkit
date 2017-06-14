from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T
from langkit.expressions import New, Property
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token
from utils import emit_and_print_errors


def run(name, prop_fn, prop_memoized):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    Diagnostics.set_lang_source_dir(os.path.abspath(__file__))

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Literal(FooNode):
        tok = Field()

    class EmptyNode(FooNode):
        pass

    class LiteralList(Literal.list_type()):
        prop = Property(prop_fn(), memoized=prop_memoized)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=foo_grammar.list_rule,
            list_rule=Row(
                '(', List(foo_grammar.list_item, sep=',', cls=LiteralList), ')'
            )[0],
            list_item=Row(Tok(Token.Number, keep=True)) ^ Literal,
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run("Not memoized", lambda: New(T.EmptyNode), False)
run("List synthetization", lambda: New(T.LiteralList), True)
print('Done')
