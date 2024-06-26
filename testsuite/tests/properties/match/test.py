from langkit.dsl import ASTNode, Field, T, abstract
from langkit.expressions import Entity, No, langkit_property

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

    @langkit_property(return_type=T.Bool, public=True)
    def nested_match():
        # We used to generate invalid Ada code for nested match expressions
        return Entity.parent.match(
            lambda _=T.Atom: True,
            lambda _=T.Expr: Entity.match(
                lambda _=T.Atom: True,
                lambda _: False,
            ),
            lambda _: False,
        )


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
    token_node = True


class Lit(Atom):
    pass


class Ref(Atom):
    pass


class Plus(Expr):
    lhs = Field()
    rhs = Field()


class Name(FooNode):
    token_node = True


class Def(FooNode):
    name = Field()
    expr = Field()


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
