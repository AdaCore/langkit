from __future__ import absolute_import, division, print_function

import langkit
from langkit.dsl import ASTNode
from langkit.lexer import Alt, Case, Lexer, Literal, Pattern
from langkit.parsers import Grammar

from lexer_example import Token
from utils import emit_and_print_errors


def test_invalid_pattern_1(l):
    l.add_rules((Pattern('['), Token.Example))


def test_invalid_pattern_2(l):
    l.add_rules((Pattern('{no_such_pattern}'), Token.Example))


def test_invalid_pattern_3(l):
    l.add_patterns(
        ('pat1', '{pat2}'),
        ('pat2', '{pat1}'),
    )
    l.add_rules((Pattern('{pat1}'), Token.Example))


def test_invalid_pattern_4(l):
    l.add_patterns(
        ('pat', '['),
    )
    l.add_rules((Pattern('{pat}'), Token.Example))


def test_invalid_rule_1(l):
    l.add_rules((Literal('def'), Token.Termination))


def test_invalid_rule_2(l):
    l.add_rules(Case(Literal('def'),
                     Alt(prev_token_cond=(Token.Var,),
                         send=Token.LexingFailure,
                         match_size=3),
                     Alt(send=Token.Def, match_size=3)))


for name, value in sorted(locals().iteritems()):
    if not name.startswith('test_'):
        continue

    print('== {} =='.format(name))

    lexer = Lexer(Token)
    value(lexer)

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        pass

    grammar = Grammar('main_rule')
    grammar.add_rules(
        main_rule=Example('example'),
    )

    emit_and_print_errors(grammar, lexer)
    langkit.reset()
    print('')

print('Done')
