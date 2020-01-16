"""
Test that Symbol bindings in the Python API are properly working.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, T, has_abstract_list
from langkit.expressions import Entity, Property, langkit_property
from langkit.parsers import Grammar, List, Or

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


foo_grammar = Grammar('main_rule')
foo_grammar.add_rules(
    main_rule=List(foo_grammar.node, list_cls=Sequence),
    node=Or(foo_grammar.example, foo_grammar.null_node),
    example=Example('example'),
    null_node=NullNode('null'),
)

build_and_run(foo_grammar, 'main.py')
print('Done')
