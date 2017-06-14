from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field
from langkit.envs import EnvSpec
from langkit.expressions import Self
from langkit.parsers import Grammar, List, Row, Tok

from lexer_example import Token
from utils import emit_and_print_errors


def lang_def():
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
    return foo_grammar

emit_and_print_errors(lang_def)

print('Done')
