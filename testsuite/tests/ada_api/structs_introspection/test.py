"""
Test that the introspection API works as expected for structs introspection.
"""

from langkit.dsl import ASTNode, Struct, T, UserField
from langkit.expressions import Property, Self

from utils import build_and_run


class Point(Struct):
    x = UserField(type=T.BigInt)
    y = UserField(type=T.BigInt)


class NodeResult(Struct):
    n = UserField(type=T.Example)


# Create a struct that is not exposed just to check that it does not show up in
# the public introspection API.
class PrivatePoint(Struct):
    x = UserField(type=T.BigInt)
    y = UserField(type=T.BigInt)


class FooNode(ASTNode):
    pass


class Example(FooNode):
    to_public = Property(lambda p=T.PrivatePoint: Point.new(x=p.x, y=p.y))

    prop = Property(
        lambda p=T.Point: Self.to_public(PrivatePoint.new(x=p.x, y=p.y)),
        public=True,
    )

    result = Property(T.NodeResult.new(n=Self), public=True)


build_and_run(lkt_file='expected_concrete_syntax.lkt', ada_main=['main.adb'])
print('Done')
