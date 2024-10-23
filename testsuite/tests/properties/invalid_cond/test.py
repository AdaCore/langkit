import langkit
from langkit.dsl import ASTNode
from langkit.expressions import Cond, Property, Self

from utils import emit_and_print_errors


def run(name, expr):
    """
    Emit and print the errors we get for the below grammar with "expr" as
    a property in BarNode.
    """

    print('== {} =='.format(name))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        cond = Property(True)
        p = Property(expr)

    emit_and_print_errors(
        lkt_file='foo.lkt', config={"lkt": {"types_from_lkt": False}}
    )
    langkit.reset()
    print('')


run('Missing args', Cond())
run('Missing args', Cond(True))
run('Missing last', Cond(Self.cond, Self))
run('Bad condition type', Cond(Self, True, False))
run('Bad return type', Cond(Self.cond, Self, False))
print('Done')
