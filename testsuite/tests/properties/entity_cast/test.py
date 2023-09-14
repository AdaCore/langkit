"""
Test that cast expressions work on entity prefixes.
"""

from langkit.dsl import ASTNode
from langkit.expressions import AbstractProperty, Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    pass


class BarNode(FooNode):
    pass


class Literal(FooNode):
    token_node = True

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)

    b = Property(Self.a.cast(BarNode.entity))

    c = Property(Self.b, public=True)

    d = Property(Self.a.cast(BarNode),
                 type=BarNode.entity,
                 public=True)


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
