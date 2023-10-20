"""
Test that basic rewriting API usage behaves as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, AbstractField, Field, NullField, abstract

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


@abstract
class Decl(FooNode):
    name = AbstractField(type=Name)
    args = AbstractField(type=Name.list)
    expr = AbstractField(type=T.Expr)


class ErrorDecl(Decl):
    error_node = True

    name = NullField()
    args = NullField()
    expr = NullField()


class Def(Decl):
    name = Field()
    args = Field()
    expr = Field()


class Var(Decl):
    name = Field()
    args = NullField()
    expr = Field()


@abstract
class Expr(FooNode):
    pass


class Literal(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


class ParenExpr(Expr):
    expr = Field()


class Plus(Expr):
    lhs = Field()
    rhs = Field()


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=[
        "general_api.adb",
        "revert.adb",
        "rewrite.adb",
        "rewrite_lists.adb",
        "rewrite_non_ascii.adb",
        "iter_units.adb",
        "apply_error.adb",
        "templates.adb",
        "preserve_formatting.adb",
        "preserve_formatting_wrap.adb",
        "clone_synthetic.adb"
    ],
    generate_unparser=True,
    types_from_lkt=False,
)
print("Done")
