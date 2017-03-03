from __future__ import (absolute_import, division, print_function,
                        unicode_literals)

from langkit.compiled_types import (
    ASTNode, Field, LexicalEnvType, Struct, T, abstract, env_metadata,
    root_grammar_class
)
from langkit.diagnostics import Diagnostics
from langkit.envs import EnvSpec, add_to_env
from langkit.expressions import Self, New, langkit_property
from langkit.parsers import Grammar, List, Opt, Row, Tok

from lexer_example import Token
from os import path
from utils import build_and_run


Diagnostics.set_lang_source_dir(path.abspath(__file__))


@env_metadata
class Metadata(Struct):
    pass


@root_grammar_class()
class FooNode(ASTNode):
    @langkit_property(public=True, return_type=LexicalEnvType)
    def env_id(env=LexicalEnvType):
        return env


@abstract
class Stmt(FooNode):
    pass


class Def(Stmt):
    id = Field()
    body = Field()

    env_spec = EnvSpec(
        add_env=True,
        add_to_env=add_to_env(New(T.env_assoc, key=Self.id.symbol, val=Self))
    )


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
