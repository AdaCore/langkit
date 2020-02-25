"""
Test that we properly warn for properties that are unreachable because they are
defined on an abstract node while all concrete subclasses have it overriden.
"""

from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Field, T, abstract
from langkit.expressions import AbstractProperty, Property

from utils import emit_and_print_errors


@abstract
class FooNode(ASTNode):
    pass


@abstract
class Expr(FooNode):

    prop1 = AbstractProperty(T.Int, public=True)

    # Warning: all concrete subclasses override this (concrete root property)
    prop2 = Property(0, public=True)

    # Warning: all concrete subclasses override this (runtime check)
    prop3 = AbstractProperty(T.Int, public=True, runtime_check=True)


@abstract
class Atom(Expr):

    # Warning: both concrete subclasses override this (concrete derived
    # property).
    prop1 = Property(1)


class Lit(Atom):
    token_node = True

    prop1 = Property(2)
    prop2 = Property(2)
    prop3 = Property(2)


class Ref(Atom):
    token_node = True

    prop1 = Property(3)
    prop2 = Property(3)
    prop3 = Property(3)


class Plus(Expr):
    lhs = Field()
    rhs = Field()

    prop1 = Property(4)
    prop2 = Property(4)
    prop3 = Property(4)


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
