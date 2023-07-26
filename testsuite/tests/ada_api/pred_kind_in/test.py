"""
Test the Kind_In node iterator predicate.
"""

from langkit.dsl import ASTNode, AbstractField, Field, NullField, T, abstract

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Decl(FooNode):
    name = AbstractField(T.Name)
    value = AbstractField(T.Expr, nullable=True)


class VarDecl(Decl):
    var_kw = Field()
    name = Field()
    value = Field()


class FunDecl(Decl):
    name = Field()
    value = NullField()


class VarKeyword(FooNode):
    token_node = True


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Number(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
