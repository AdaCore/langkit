from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.expressions import Entity, No, langkit_property
from langkit.parsers import Grammar, List, Or, Pick

from lexer_example import Token
from utils import build_and_run


@abstract
class FooNode(ASTNode):

    # Expected sorted kinds: [Def, Lit, Ref, Plus, FooNode.list]

    @langkit_property(return_type=T.Atom.entity.array, public=True)
    def find_atoms():
        return Entity.match(
            # This should match Lit .. Ref
            lambda a=T.Atom: a.singleton,

            # This should match Def | Plus .. FooNode.list
            lambda _: No(T.Atom.entity.array),
        ).concat(Entity.children.mapcat(lambda c: c.find_atoms))

    @langkit_property(return_type=T.Expr.entity.array, public=True)
    def find_atoms_or_exprs():
        return Entity.match(
            # This should match Lit .. Ref
            lambda a=T.Atom: a.cast(T.Expr).singleton,

            # This should match the only remaining expression: Plus
            lambda e=T.Expr: e.singleton,

            # This should match Def | FooNode.list
            lambda _: No(T.Expr.entity.array),
        ).concat(Entity.children.mapcat(lambda c: c.find_atoms_or_exprs))

    @langkit_property(return_type=T.Expr.entity.array, public=True)
    def find_exprs():
        return Entity.match(
            # This should match Lit .. Plus
            lambda e=T.Expr: e.singleton,

            # This should match nothing (Atom is an Expr subclass), so emit a
            # warning.
            lambda a=T.Atom: a.cast(T.Expr).singleton,

            # This should match Def | FooNode.list
            lambda _: No(T.Expr.entity.array),
        ).concat(Entity.children.mapcat(lambda c: c.find_exprs))


@abstract
class Expr(FooNode):

    @langkit_property(return_type=T.Ref.entity.array, public=True)
    def find_refs():
        return Entity.match(
            # This should match Ref
            lambda r=T.Ref: r.singleton,

            # This should match Lit
            lambda _: No(T.Ref.entity.array),

            # ... and we should not get CASE coverage errors in generated code,
            # even though we did not cover all possible FooNode kinds.
        ).concat(Entity.children.mapcat(
            lambda c: c.cast_or_raise(T.Expr).find_refs
        ))


@abstract
class Atom(Expr):
    pass


class Lit(Atom):
    tok = Field()


class Ref(Atom):
    tok = Field()


class Plus(Expr):
    lhs = Field()
    rhs = Field()


class Def(FooNode):
    name = Field()
    expr = Field()


grammar = Grammar('main_rule')
grammar.add_rules(
    main_rule=List(Or(
        Def('def', Token.Identifier, grammar.expr),
        grammar.expr
    )),

    expr=Or(grammar.atom, grammar.plus),

    atom=Or(grammar.lit, grammar.ref),
    lit=Lit(Token.Number),
    ref=Ref(Token.Identifier),

    plus=Pick('(', Plus(grammar.expr, '+', grammar.expr), ')'),
)
build_and_run(grammar, 'main.py')
print('Done')
