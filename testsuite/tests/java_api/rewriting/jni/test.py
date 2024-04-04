"""
Test that basic rewriting API usage behaves as expected.
"""

from langkit.compiled_types import T
from langkit.dsl import (
    ASTNode, AbstractField, Annotations, Field, NullField, abstract
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


class Name(FooNode):
    token_node = True
    annotations = Annotations(custom_short_image=True)


@abstract
class Decl(FooNode):
    name = AbstractField(type=Name)
    expr = AbstractField(type=T.Literal)


class ErrorDecl(Decl):
    error_node = True

    name = NullField()
    expr = NullField()


class Var(Decl):
    name = Field()
    expr = Field()


class Literal(FooNode):
    token_node = True
    annotations = Annotations(custom_short_image=True)


build_and_run(
    lkt_file="expected_concrete_syntax.lkt",
    java_main="RewritingTests",
    generate_unparser=True,
    types_from_lkt=True,
)
print("Done")
