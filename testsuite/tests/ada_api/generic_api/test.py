"""Check that the generic Ada API works as expected."""

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


class Ref(Expr):
    name = Field()


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
