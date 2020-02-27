from __future__ import absolute_import, division, print_function

from langkit.dsl import ASTNode, Bool, Field, abstract
from langkit.expressions import Literal, Property, Self

from utils import emit_and_print_errors


def run(name, match_expr):
    """
    Emit and print the errors we get for the below grammar with "match_expr" as
    a property in ExampleNode.
    """

    global BodyNode, Compound, Expression, FooNode, NullNode, Number

    print('== {} =='.format(name))

    @abstract
    class FooNode(ASTNode):
        prop = Property(Literal(0), public=True)

    @abstract
    class BodyNode(FooNode):
        pass

    class NullNode(BodyNode):
        pass

    @abstract
    class Expression(BodyNode):
        pass

    class Number(Expression):
        token_node = True

    class Compound(Expression):
        prefix = Field()
        suffix = Field()

    class ExampleNode(FooNode):
        body = Field()

        prop = Property(match_expr)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


# Incomplete set of matchers
run('Missing NullNode', lambda: Self.body.match(
    lambda e=Expression: e.prop,
))
run('Missing Expression', lambda: Self.body.match(
    lambda e=NullNode: e.prop,
))
run('Missing Compound', lambda: Self.body.match(
    lambda e=NullNode: e.prop,
    lambda e=Number:   e.prop,
))

# Invalid matchers
run('Invalid type', lambda: Self.body.match(
    lambda e=NullNode:   e.prop,
    lambda e=Expression: e.prop,
    lambda e=Bool:       e.prop,
))
run('Irrelevant AST node', lambda: Self.body.match(
    lambda e=NullNode:   e.prop,
    lambda e=Expression: e.prop,
    lambda e=FooNode:    e.prop,
))

# Unreachable matchers
run('Default case after full coverage', lambda: Self.body.match(
    lambda e=BodyNode: e.prop,
    lambda _:          Literal(1),
))

run('Node after default case (1)', lambda: Self.body.match(
    lambda _:          Literal(0),
    lambda e=BodyNode: e.prop,
))
run('Node after default case (2)', lambda: Self.body.match(
    lambda _:        Literal(0),
    lambda e=Number: e.prop,
))

run('Node after full coverage (1)', lambda: Self.body.match(
    lambda e=BodyNode: e.prop,
    lambda e=Number:   e.prop,
))
run('Node after full coverage (2)', lambda: Self.body.match(
    lambda e=NullNode: e.prop,
    lambda e=Number:   e.prop,
    lambda e=Compound: e.prop,
    lambda e=BodyNode: e.prop,
))

print('Done')
