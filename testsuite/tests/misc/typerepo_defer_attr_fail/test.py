from langkit.dsl import ASTNode, Field, T

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class Example(FooNode):
    tok = Field(type=T.FooNode.does_not_exist)


class ExampleField(FooNode):
    pass


emit_and_print_errors(
    lkt_file='foo.lkt', config={"lkt_spec": {"types_from_lkt": False}}
)
print('Done')
