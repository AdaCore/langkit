from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Self
from langkit.parsers import Grammar, List, Pick

from lexer_example import Token
from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()
    body = Field()
    env_spec = EnvSpec(add_env(), add_to_env_kv(Self.name, Self))


grammar = Grammar('stmt_rule')
grammar.add_rules(
    def_rule=Def(
        Token.Identifier,
        '(', grammar.stmt_rule, ')'
    ),
    stmt_rule=List(
        grammar.def_rule
        | Pick('{', List(grammar.stmt_rule, empty_valid=True), '}'),
        empty_valid=True
    )
)
emit_and_print_errors(grammar)

print('Done')
