"""
Test that the lowering pass for properties dispatching works properly rewrites
references to all transformed properties.
"""

from __future__ import absolute_import, division, print_function

import os.path

from langkit.diagnostics import Diagnostics
from langkit.dsl import ASTNode, Field, T, abstract, synthetic
from langkit.envs import EnvSpec, add_env, add_to_env, reference
from langkit.expressions import New, No, Self, langkit_property
from langkit.parsers import Grammar, List, Or, Pick, Tok

from lexer_example import Token
from utils import build_and_run


Diagnostics.set_lang_source_dir(os.path.abspath(__file__))


@abstract
class FooNode(ASTNode):
    pass


@abstract
class Expr(FooNode):
    pass


@abstract
class Atom(Expr):
    pass


class Lit(Atom):
    tok = Field()


class Ref(Atom):
    tok = Field()

    @langkit_property(public=True)
    def resolve():
        return Self.node_env.get_first(Self.tok.symbol)

    @langkit_property(public=True)
    def dummy():
        return Self.referenced_env.get(Self.tok.symbol)

    @langkit_property()
    def referenced_env():
        return No(T.LexicalEnvType)


@synthetic
class MiddleRef(Ref):

    @langkit_property()
    def referenced_env():
        return No(T.LexicalEnvType)


class DerivedRef(MiddleRef):
    @langkit_property()
    def referenced_env():
        return Self.unit.root.node_env.get_first(Self.tok.symbol).children_env


class Plus(Expr):
    lhs = Field()
    rhs = Field()


class Var(FooNode):
    name = Field()
    expr = Field()

    env_spec = EnvSpec(
        add_to_env(mappings=New(T.env_assoc, key=Self.name.symbol, val=Self)),
    )


class Def(FooNode):
    name = Field()
    imports = Field()
    vars = Field()
    expr = Field()

    env_spec = EnvSpec(
        add_to_env(mappings=New(T.env_assoc, key=Self.name.symbol, val=Self)),
        add_env(),
        reference(Self.imports.map(lambda i: i.cast(T.FooNode)),

                  # If PropertyDef rewriting omits the following references,
                  # env lookup will never reach DerivedRef.referenced_env, so
                  # resolution will sometimes fail to reach definition.
                  T.MiddleRef.referenced_env)
    )


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(Or(
        Def('def', Tok(Token.Identifier, keep=True),
            grammar.imports, grammar.vars, grammar.expr),
        grammar.expr
    )),

    imports=Pick('(', List(grammar.derived_ref, empty_valid=True), ')'),

    var=Var(Tok(Token.Identifier, keep=True), '=', grammar.expr),
    vars=Pick('{', List(grammar.var, empty_valid=True), '}'),

    expr=Or(grammar.atom, grammar.plus),

    atom=Or(grammar.lit, grammar.ref),
    lit=Lit(Tok(Token.Number, keep=True)),
    ref=Ref(Tok(Token.Identifier, keep=True)),
    derived_ref=DerivedRef(Tok(Token.Identifier, keep=True)),

    plus=Pick('(', Plus(grammar.expr, '+', grammar.expr), ')'),
)
build_and_run(grammar, 'main.py')
print('Done')
