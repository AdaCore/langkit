from langkit.dsl import ASTNode, T
from langkit.expressions import langkit_property

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):

    @langkit_property(public=True)
    def compare(lhs=T.Int, rhs=T.BigInt):
        return lhs < rhs


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
