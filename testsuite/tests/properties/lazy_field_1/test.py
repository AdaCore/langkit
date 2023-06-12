"""
Regression test. We used to generate bad Ada code for the initializer of a
class for which one of the ancestor has a lazy field.
"""

from langkit.compiled_types import T
from langkit.dsl import ASTNode, Field, abstract
from langkit.expressions import lazy_field

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Expr(FooNode):
    @lazy_field(public=True)
    def my_field():
        return 42


class IntLit(Expr):
    token_node = True


class AddExpr(Expr):
    left = Field(type=T.Expr)
    right = Field(type=T.Expr)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    gpr_mains=["main.adb"],
    types_from_lkt=True,
)
print("Done")
