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


class Ref(Expr):
    null_qual = Field()
    name = Field()


class Addition(Expr):
    lhs = Field()
    rhs = Field()


class Number(Expr):
    token_node = True


class Null(FooNode):
    enum_node = True
    qualifier = True


# Do not load node types from Lkt to enforce unsorted node creation (Ref
# created before Number), which used to have an unexpected influence on the
# order of nodes in the introspection API, leading to bugs on code that expect
# the hierarchical sorting of node types.
build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
