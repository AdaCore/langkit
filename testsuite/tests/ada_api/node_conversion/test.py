"""
Test that the introspection API works as expected for queries related to node
types.
"""

from langkit.dsl import ASTNode, Field, abstract

from utils import build_and_run


class FooNode(ASTNode):
    pass


class VarDecl(FooNode):
    name = Field()
    value = Field()


class Name(FooNode):
    token_node = True


@abstract
class Expr(FooNode):
    pass


class Addition(Expr):
    lhs = Field()
    rhs = Field()


class Number(Expr):
    token_node = True


class Null(FooNode):
    enum_node = True
    qualifier = True


class Ref(Expr):
    null_qual = Field()
    name = Field()


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    gpr_mains=['main.adb'],
    types_from_lkt=True,
)
print('Done')
