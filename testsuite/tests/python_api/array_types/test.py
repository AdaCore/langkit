"""
Test that Symbol bindings in the Python API are properly working.
"""

from langkit.dsl import ASTNode, T, has_abstract_list
from langkit.expressions import Entity, Property, langkit_property

from utils import build_and_run


@has_abstract_list
class FooNode(ASTNode):

    @langkit_property(public=True)
    def count(seq=T.Example.entity.array):
        return seq.length


class Sequence(FooNode.list):
    all_items = Property(Entity.map(lambda i: i), public=True)
    example_items = Property(Entity.filtermap(
        lambda i: i.cast_or_raise(T.Example),
        lambda i: i.is_a(T.Example)
    ), public=True)


class Example(FooNode):
    pass


class NullNode(FooNode):
    pass


build_and_run(
    lkt_file='expected_concrete_syntax.lkt',
    py_script='main.py',
    types_from_lkt=True,
)
print('Done')
