from langkit.dsl import ASTNode, Field, abstract
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


def run(name, expr_fn):
    """
    Emit and print the errors we get for the below grammar with "expr_fn" as a
    property in Example.
    """
    print('== {} =='.format(name))

    @abstract
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        name = Field()

        prop = Property(expr_fn)

    class Name(FooNode):
        token_node = True

    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


run('No argument', Self.name.then(lambda: Self))
run('Two arguments', Self.name.then(lambda x, y: x.foo(y)))
run('Default value', Self.name.then(lambda name=None: name))

print('Done')
