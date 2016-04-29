from langkit.compile_context import CompileCtx
from langkit.compiled_types import (
    ASTNode, Field, Struct, abstract, env_metadata, root_grammar_class
)
from langkit.envs import EnvSpec
from langkit.expressions import Property, Self
from langkit.parsers import Grammar, List, Opt, Row, Tok

from lexer_example import Token, foo_lexer
from utils import build_and_run, prepare_context


@env_metadata
class Metadata(Struct):
    pass


@root_grammar_class
class FooNode(ASTNode):
    pass


@abstract
class Stmt(FooNode):
    pass


class Def(Stmt):
    id = Field()
    body = Field()

    env_spec = EnvSpec(add_env=True, add_to_env=(Self.id.symbol, Self))


class Block(Stmt):
    items = Field()

    env_spec = EnvSpec(add_env=True)


foo_grammar = Grammar('stmts_rule')
foo_grammar.add_rules(
    def_rule=Row(
        Tok(Token.Identifier, keep=True),
        Opt(Row('(', foo_grammar.stmts_rule, ')')[1])
    ) ^ Def,

    stmt_rule=(
        foo_grammar.def_rule
        | Row('{', List(foo_grammar.stmt_rule, empty_valid=True), '}') ^ Block
    ),

    stmts_rule=List(foo_grammar.stmt_rule)
)


build_and_run(foo_grammar, 'script.py', library_fields_all_public=True)
