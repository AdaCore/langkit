"""
Test creating an array literal whose element_type is the node type
defined by the enclosing class.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import (
    ArrayLiteral, Entity, langkit_property
)

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    @langkit_property(public=True, return_type=T.Example.entity.array)
    def entities_array():
        return ArrayLiteral(
            [Entity, Entity, Entity],
            element_type=Example.entity
        )


emit_and_print_errors(lkt_file="expected_concrete_syntax.lkt")
print('Done')
