"""
Check that the Case lexing rule works as expected.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, abstract

from utils import build_and_run, unparse_all_script


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


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              unparse_script=unparse_all_script)
print('Done')
