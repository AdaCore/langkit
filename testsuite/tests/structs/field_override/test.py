"""
Test the handling of analysis units in the properties DSL.
"""

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


@abstract
class RootNode(FooNode):
    name = Property(Self.match(
        lambda e=T.Expr: e.name,
        lambda n=T.Name: n
    ))


class Expr(RootNode):
    name = Field()


class Name(RootNode):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
