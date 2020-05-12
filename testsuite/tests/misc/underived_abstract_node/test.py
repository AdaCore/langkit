from langkit.dsl import ASTNode, abstract

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    pass


@abstract
class AbstractNode(Example):
    pass


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
