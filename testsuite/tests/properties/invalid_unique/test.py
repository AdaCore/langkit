"""
Test the "unique" array operation in the DSL (invalid usages).
"""

from langkit.dsl import ASTNode, T
from langkit.expressions import Self, langkit_property

from utils import emit_and_print_errors


def test_invalid(label, expr):
    print('== {} =='.format(label))

    class FooNode(ASTNode):
        pass

    class Example(FooNode):
        token_node = True

        @langkit_property(public=True, return_type=T.Bool)
        def test():
            return expr.unique

    emit_and_print_errors(lkt_file='foo.lkt')


test_invalid('Not an array', Self)
test_invalid('Not hashable array element', Self.token_start.singleton)
print('Done')
