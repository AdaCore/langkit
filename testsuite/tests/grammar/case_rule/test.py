"""
Check that the Case lexing rule works as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract
from langkit.lexer import (Alt, Case, Lexer, LexerToken, Literal, Pattern,
                           WithText)

from utils import build_and_run, unparse_all_script


class Token(LexerToken):
    Dot = WithText()
    Id = WithText()
    Char = WithText()
    Tick = WithText()
    Newline = WithText()


foo_lexer = Lexer(Token)
foo_lexer.add_rules(
    (Literal('.'), Token.Dot),
    (Pattern('[a-zA-Z]+'), Token.Id),
    (Literal('\''), Token.Tick),
    (Pattern(r'\n'), Token.Newline),

    Case(Pattern('\'.\''),
         Alt(prev_token_cond=(Token.Id, ),
             send=Token.Tick,
             match_size=1),
         Alt(send=Token.Char, match_size=3))
)


class FooNode(ASTNode):
    pass


@abstract
class Expr(FooNode):
    pass


class Name(Expr):
    token_node = True


class CharLit(Expr):
    token_node = True


class DotExpr(Expr):
    prefix = Field(type=Expr)
    suffix = Field(type=Name)


class AttrRef(Expr):
    prefix = Field(type=Expr)
    name = Field(type=Name)


build_and_run(lkt_file='expected_concrete_syntax.lkt', lexer=foo_lexer,
              py_script='main.py', unparse_script=unparse_all_script)
print('Done')
