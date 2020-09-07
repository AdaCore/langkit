from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property, lazy_field

from utils import emit_and_print_errors


def run(name, create_nodes):
    """
    Emit and print the errors we get for the "foo.lkt" grammar after creating
    the nodes with the "create_nodes" callback.
    """

    print('== {} =='.format(name))
    create_nodes()
    emit_and_print_errors(lkt_file='foo.lkt')
    print('')


def test1_inconsistent_base():
    class FooNode(ASTNode):
        @langkit_property(public=True)
        def p():
            return 1

    class Example(FooNode):
        @lazy_field()
        def p():
            return 2

    class Number(FooNode):
        pass


def test2_inconsistent_base():
    class FooNode(ASTNode):
        @lazy_field(public=True)
        def p():
            return 1

    class Example(FooNode):
        @langkit_property()
        def p():
            return 2

    class Number(FooNode):
        pass


def test3_with_args():
    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        @lazy_field(public=True)
        def p(b=T.Bool):
            return b

    class Number(FooNode):
        pass


for key, value in sorted(locals().items()):
    if key.startswith('test'):
        run(key, value)
print('Done')
