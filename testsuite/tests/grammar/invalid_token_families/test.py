"""
Test that grammars which are troublesome for the unparsing machinery are
properly detected.
"""

from langkit.dsl import ASTNode
from langkit.lexer import (Lexer, LexerToken, Pattern, TokenFamily, WithSymbol,
                           WithText)

from utils import emit_and_print_errors


class BaseToken(LexerToken):
    Number = WithText()
    Identifier = WithSymbol()


def run(token_cls):
    print('== {} =='.format(token_cls.__name__))

    class FooNode(ASTNode):
        pass

    class Identifier(FooNode):
        token_node = True

    class Number(FooNode):
        token_node = True

    foo_lexer = Lexer(token_cls)
    foo_lexer.add_rules(
        (Pattern('[0-9]+'),                 token_cls.Number),
        (Pattern('[a-zA-Z_][a-zA-Z0-9_]*'), token_cls.Identifier),
    )

    emit_and_print_errors(lkt_file='foo.lkt', lexer=foo_lexer,
                          generate_unparser=True)

    BaseToken.Number.name = None
    BaseToken.Identifier.name = None
    print('')


class InvalidToken(BaseToken):
    Alphanumericals = TokenFamily(BaseToken.Number, BaseToken.Identifier,
                                  'foobar')
run(InvalidToken)


class PresentTwice(BaseToken):
    Alphanumericals = TokenFamily(BaseToken.Number, BaseToken.Identifier)
    Numbers = TokenFamily(BaseToken.Number)
run(PresentTwice)

print('Done')
