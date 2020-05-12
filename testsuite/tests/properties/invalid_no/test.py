from langkit.dsl import ASTNode, Int
from langkit.expressions import No, Property

from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    global FooNode

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        prop = Property(expr, public=True)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run("Correct code", lambda: No(FooNode.entity))
run("Incorrect No usage", lambda: No(Int))
print('Done')
