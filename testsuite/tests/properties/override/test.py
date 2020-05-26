"""
Test that when a property B overrides a property A, B's return type must be a
subtype of A's.
"""

from langkit.dsl import ASTNode, T, abstract
from langkit.expressions import AbstractProperty, No, Property, Self

from utils import emit_and_print_errors


def run(name, astnode_fn):
    """
    Emit and print the errors we get for the below grammar with "match_expr" as
    a property in ExampleNode.
    """

    print('== {} =='.format(name))

    astnode = astnode_fn(T)

    @abstract
    class FooNode(ASTNode):
        pass

    @abstract
    class MiddleNode(FooNode):
        get_random_node = AbstractProperty(type=T.MiddleNode)
        public_prop = Property(Self.get_random_node.as_bare_entity,
                               public=True)

    class ExampleNode(MiddleNode):
        get_random_node = Property(No(astnode))

    class NullNode(FooNode):
        pass

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


# Incomplete set of matchers
run('FooNode', lambda T: T.FooNode)
run('MiddleNode', lambda T: T.MiddleNode)
run('NullNode', lambda T: T.NullNode)

print('Done')
