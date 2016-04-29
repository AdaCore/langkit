from langkit.compiled_types import (
    ASTNode, Field, root_grammar_class
)
from langkit.envs import EnvSpec
from langkit.expressions import Self
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token, foo_lexer
from utils import emit_and_print_errors


@root_grammar_class
class FooNode(ASTNode):
    pass


class Def(FooNode):
    name = Field()
    body = Field()

    env_spec = EnvSpec(add_env=True, add_to_env=(Self.name, Self))


foo_grammar = Grammar('stmt_rule')
foo_grammar.add_rules(
    def_rule=Row(
        Tok(Token.Identifier, keep=True),
        '(', foo_grammar.stmt_rule, ')'
    ) ^ Def,
    stmt_rule=List(
        foo_grammar.def_rule
        | Row('{', List(foo_grammar.stmt_rule, empty_valid=True), '}')[1],
        empty_valid=True
    )
)
assert emit_and_print_errors(foo_grammar)

print 'Done'
