"""
Check that the Case lexing rule works as expected.
"""

from langkit.dsl import ASTNode, Field, abstract

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Expr(FooNode):
    pass


class Name(Expr):
    token_node = True


class CharLit(Expr):
    token_node = True


class DotExpr(Expr):
    prefix = Field(type=Expr)
    suffix = Field(type=Name)


class AttrRef(Expr):
    prefix = Field(type=Expr)
    name = Field(type=Name)


build_and_run(
    lkt_file='lexer_parser.lkt',
    py_script='main.py',
    unparse_script="to:expected_concrete_syntax.lkt,nodes",
    types_from_lkt=True,
)
print('Done')
