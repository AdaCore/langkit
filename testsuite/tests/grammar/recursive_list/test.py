from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec
from langkit.expressions import Self
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()
    body = Field()
    env_spec = EnvSpec(add_env=True, add_to_env=(Self.name, Self))


grammar = Grammar('stmt_rule')
grammar.add_rules(
    def_rule=Row(
        Tok(Token.Identifier, keep=True),
        '(', grammar.stmt_rule, ')'
    ) ^ Def,
    stmt_rule=List(
        grammar.def_rule
        | Row('{', List(grammar.stmt_rule, empty_valid=True), '}')[1],
        empty_valid=True
    )
)
emit_and_print_errors(grammar)

print('Done')
