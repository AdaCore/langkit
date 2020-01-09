"""
Check that trivia are properly scanned when the lexer tracks indentation.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.lexer import (Ignore, Lexer, LexerToken, Literal, Pattern,
                           WithSymbol, WithText, WithTrivia)
from langkit.parsers import Grammar, List, Or, _

from utils import build_and_run


class Token(LexerToken):
    Def = WithText()
    LPar = WithText()
    RPar = WithText()
    Comma = WithText()
    Identifier = WithSymbol()
    Comment = WithTrivia()

foo_lexer = Lexer(Token, track_indent=True)
foo_lexer.add_rules(
    (Pattern(r'[ \r\t]+'), Ignore()),

    (Literal('def'), Token.Def),
    (Literal(','),   Token.Comma),
    (Literal('('),   Token.LPar),
    (Literal(')'),   Token.RPar),

    (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), Token.Identifier),

    (Pattern('#.*'), Token.Comment),
)
L = foo_lexer


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()
    stmts = Field()


class Identifier(FooNode):
    token_node = True


class Call(FooNode):
    name = Field()
    args = Field()


class Indented(FooNode):
    inner = Field()


class Newline(FooNode):
    token_node = True

foo_grammar = Grammar('main_rule')
G = foo_grammar


def newlines():
    return _(List(G.newline, empty_valid=True))


foo_grammar.add_rules(
    newline=Newline(L.Newline),
    expr=Or(G.call, G.identifier, G.indented),

    identifier=Identifier(Token.Identifier),
    call=Call(G.identifier, '(',
              newlines(),
              List(G.call, sep=',', empty_valid=True),
              newlines(),
              ')'),
    indented=Indented(newlines(), L.Indent,
                      List(newlines(), G.expr, newlines(), sep=L.Newline),
                      L.Dedent),

    def_node=Def('def', G.identifier, L.Newline, List(G.indented)),

    main_rule=List(newlines(), G.def_node, newlines())
)
build_and_run(foo_grammar, 'main.py', lexer=foo_lexer)
print('Done')
