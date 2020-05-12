from langkit.dsl import ASTNode, Field, Int
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode, BarNode, ListNode

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        list_node = Field()

    class ListNode(FooNode):
        nb_list = Field()
        bar_node_parent = Property(Self.parent.cast(BarNode))
        prop = Property(expr)

        public_bar_node_parent = Property(Self.bar_node_parent.as_bare_entity,
                                          public=True)
        public_prop = Property(Self.prop.as_bare_entity, public=True)

    class NumberNode(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run("Correct code", lambda: Self.parent.cast(BarNode))
run("Invalid cast 1", lambda: Self.parent.cast(Int))
run("Invalid cast 2", lambda: Self.bar_node_parent.cast(ListNode))
print('Done')
