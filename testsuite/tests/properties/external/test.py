"""
Test that external properties build and run properly.
"""

from langkit.dsl import ASTNode, Field, Int, abstract
from langkit.expressions import (
    AbstractProperty, ExternalProperty, Property, Self
)

from utils import build_and_run


class FooNode(ASTNode):
    pass


@abstract
class Expression(FooNode):
    result = AbstractProperty(type=Int, public=True)


class Literal(Expression):
    token_node = True

    result = ExternalProperty(uses_entity_info=False, uses_envs=False)


class Plus(Expression):
    left = Field()
    right = Field()

    result = Property(Self.left.result + Self.right.result)


build_and_run(lkt_file='expected_concrete_syntax.lkt', py_script='main.py',
              types_from_lkt=True)
print('Done')
