"""
Test that we properly warn for properties that are unreachable because they are
defined on an abstract node while all concrete subclasses have it overriden.
"""

from langkit.dsl import ASTNode
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


class FooNode(ASTNode):

    # We should have no warnings for the whole subgraph of properties here,
    # since the root of the callgraph (the "c" property) has
    # "warn_on_unused=False".

    a = Property(1)
    b = Property(Self.a)
    c = Property(Self.b, warn_on_unused=False)

    # We should get a warning for this one though, which is out of the
    # subgraph.
    d = Property(Self.c)


class Node(FooNode):
    token_node = True


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
