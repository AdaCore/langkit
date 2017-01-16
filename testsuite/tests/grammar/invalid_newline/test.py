"""
Test that Bind works when binding from env elements.
"""

import os.path

from langkit.compiled_types import ASTNode, Field, root_grammar_class
from langkit.diagnostics import Diagnostics
from langkit.lexer import (
    Eof, Ignore, Lexer, LexerToken, Literal, Pattern, WithSymbol, WithText
)
from langkit.parsers import Grammar, Row, Tok, nl, ind, ded, Or

from utils import emit_and_print_errors


class Token(LexerToken):
    Example = WithText()
    Null = WithText()

    Comma = WithText()
    LPar = WithText()
    RPar = WithText()
    LBrace = WithText()
    RBrace = WithText()
    Plus = WithText()

    Number = WithText()
    Identifier = WithSymbol()

foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Pattern(r'[ \n\r\t]+'), Ignore()),
    (Eof(),                  Token.Termination),

    (Literal("example"),     Token.Example),
    (Literal("null"),        Token.Null),

    (Literal(','),           Token.Comma),
    (Literal('('),           Token.LPar),
    (Literal(')'),           Token.RPar),
    (Literal('{'),           Token.LBrace),
    (Literal('}'),           Token.RBrace),
    (Literal('+'),           Token.Plus),

    (Pattern('[0-9]+'),      Token.Number),
    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),
)

Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@root_grammar_class()
class FooNode(ASTNode):
    pass


class Literal(FooNode):
    tok = Field()


class NewLineNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()


class IndentNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()


class CompositeNode(FooNode):
    lit_1 = Field()
    lit_2 = Field()
    lit_3 = Field()
    lit_4 = Field()


def grammar():
    foo_grammar = Grammar('main_rule')
    A = foo_grammar

    foo_grammar.add_rules(
        lit=Row(Tok(Token.Number, keep=True)) ^ Literal,
        nl=NewLineNode(A.lit, nl(), A.lit),
        ind=IndentNode(A.lit, nl(), ind(), A.lit),
        comp=CompositeNode(
            A.lit, nl(), A.lit, ind(), A.lit, nl(), ded(), A.lit
        ),
        main_rule=Or(A.comp, A.ind, A.nl)
    )
    return foo_grammar

emit_and_print_errors(grammar, lexer=foo_lexer)
print 'Done'
