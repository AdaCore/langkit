from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, Struct, abstract, env_metadata
from langkit.envs import EnvSpec, add_env, add_to_env_kv
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, List, Opt

from lexer_example import Token
from utils import emit_and_print_errors


@env_metadata
class Metadata(Struct):
    pass


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


@abstract
class Stmt(FooNode):
    pass


class Def(Stmt):
    id = Field()
    body = Field()

    env_spec = EnvSpec(
        add_to_env_kv(Self.id.symbol, Self),
        add_env()
    )

    faulty_prop = Property(Self._env_mappings_0)


class Block(Stmt):
    items = Field()

    env_spec = EnvSpec(add_env())


grammar = Grammar('stmts_rule')
grammar.add_rules(
    def_rule=Def(
        Name(Token.Identifier),
        Opt('(', grammar.stmts_rule, ')')
    ),

    stmt_rule=(
        grammar.def_rule
        | Block('{',
                List(grammar.stmt_rule, empty_valid=True),
                '}')
    ),

    stmts_rule=List(grammar.stmt_rule)
)
emit_and_print_errors(grammar)
print('Done')
