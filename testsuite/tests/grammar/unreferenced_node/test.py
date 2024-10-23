from langkit.dsl import ASTNode, Field, abstract

from utils import emit_and_print_errors


@abstract
class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    pass


class UnreferencedNode(FooNode):
    untyped_field = Field()


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt": {"types_from_lkt": False}}
)
print('Done')
