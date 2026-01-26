from langkit.dsl import ASTNode
from langkit.expressions import Let, Literal, Property

from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in ExampleNode.
    """

    global FooNode

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class BarNode(FooNode):
        prop = Property(expr, public=True)

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run("Correct code", lambda: Let(lambda a=Literal(1): a))
run("Missing var value", lambda: Let(lambda a: a))
run("Invalid args", lambda: Let(lambda a=Literal(1), *b, **c: a))

print('Done')
