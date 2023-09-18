"""
Check that match expression on entity types works properly.
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import AbstractProperty, Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    get_num = AbstractProperty(T.Int)


class Example(FooNode):
    get_num = Property(2)


class Literal(FooNode):
    token_node = True

    get_num = Property(3)

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)

    b = Property(
        Self.a.match(
            lambda e=Example.entity: e.get_num,
            lambda c=FooNode.entity: c.get_num,
        ),
        public=True
    )

    c = Property(
        Self.a.match(
            lambda e=Example: e.get_num,
            lambda c=FooNode: c.get_num,
        ),
        public=True
    )


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
