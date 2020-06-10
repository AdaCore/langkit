"""
Test that a custom rule can be used to parse an analysis unit.
"""

from langkit.dsl import ASTNode, Field, abstract

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True


class Def(FooNode):
    name = Field()
    args = Field()
    expr = Field()


@abstract
class Expr(FooNode):
    pass


class Literal(Expr):
    token_node = True


class Ref(Expr):
    name = Field()


class ParentExpr(Expr):
    expr = Field()


class Plus(Expr):
    lhs = Field()
    rhs = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              generate_unparser=True, types_from_lkt=True)
print('Done')
