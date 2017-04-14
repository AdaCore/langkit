from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit.compiled_types import ASTNode, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.expressions import Literal, Property, Self
from langkit.parsers import Grammar, Tok

from lexer_example import Token
from os import path
from utils import emit_and_print_errors


def run(name, lhs, rhs):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in Example.
    """

    global FooNode, BarNode, ListNode

    Diagnostics.set_lang_source_dir(path.abspath(__file__))

    print('== {} =='.format(name))

    @root_grammar_class()
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        prop = Property(lhs.equals(rhs), public=True)

    def lang_def():
        foo_grammar = Grammar('main_rule')
        foo_grammar.add_rules(
            main_rule=Example(Tok(Token.Example)),
        )
        return foo_grammar

    emit_and_print_errors(lang_def)
    print('')


run('Correct code', Self, Self.parent)
run('Boolean <-> ASTNode', Literal(True), Self)
run('ASTNode <-> Boolean', Self, Literal(True))
run('Long <-> Boolean', Literal(0), Literal(True))
print('Done')
