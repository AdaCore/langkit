from langkit.dsl import ASTNode, T
from langkit.expressions import Property, Self

from utils import emit_and_print_errors


class FooNode(ASTNode):
    pass


class ExampleNode(FooNode):
    prop = Property(Self.cast(T.ExampleNode).as_bare_entity, public=True)


emit_and_print_errors(lkt_file='foo.lkt')
print('Done')
