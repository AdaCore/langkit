"""
Test that Bind works when binding from entities.
"""

from langkit.dsl import ASTNode, Int, LogicVar, UserField
from langkit.expressions import AbstractProperty, Bind, Let, Property, Self

from utils import build_and_run


class FooNode(ASTNode):
    prop = AbstractProperty(runtime_check=True, type=Int, public=True)


class Literal(FooNode):
    token_node = True

    a = AbstractProperty(runtime_check=True, type=FooNode.entity)
    var = UserField(LogicVar, public=False)

    b = Property(Bind(Self.var, Self.a))

    public_prop = Property(Let(lambda _=Self.b: Self.as_bare_entity),
                           public=True)


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
